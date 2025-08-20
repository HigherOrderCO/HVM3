module Main where

import Network.Socket as Network
import System.IO (hSetEncoding, utf8, hPutStrLn, stderr)
import Control.Exception (try, fromException, SomeException, finally, AsyncException(UserInterrupt))
import Control.Monad (when, forM_, unless)
import Data.List (partition, isPrefixOf, find)
import HVM.API
import HVM.Collapse
import HVM.Extract
import HVM.Foreign
import HVM.Parse
import HVM.Reduce
import HVM.Type
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO
import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array (peekArray)
import Codec.Picture
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Map.Strict as MS

-- Main
-- ----

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    ("run" : file : rest) -> do
      let (flags, sArgs) = partition ("-" `isPrefixOf`) rest
      let compiled       = "-c" `elem` flags
      let collapseFlag   = Data.List.find (isPrefixOf "-C") flags >>= parseCollapseFlag
      let stats          = "-s" `elem` flags
      let heatmap        = "-h" `elem` flags
      let debug          = "-d" `elem` flags
      let hideQuotes     = "-Q" `elem` flags
      let mode           = case collapseFlag of { Just n -> Collapse n ; Nothing -> Normalize }
      cliRun file debug compiled mode stats hideQuotes heatmap sArgs
    ("serve" : file : rest) -> do
      let (flags, _)     = partition ("-" `isPrefixOf`) rest
      let compiled       = "-c" `elem` flags
      let collapseFlag   = Data.List.find (isPrefixOf "-C") flags >>= parseCollapseFlag
      let stats          = "-s" `elem` flags
      let heatmap        = "-h" `elem` flags
      let debug          = "-d" `elem` flags
      let hideQuotes     = "-Q" `elem` flags
      let mode           = case collapseFlag of { Just n -> Collapse n ; Nothing -> Normalize }
      cliServe file debug compiled mode stats hideQuotes -- heatmap not supported in serve
    ["help"] -> printHelp
    _ -> printHelp
  case result of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right _ -> do
      exitWith ExitSuccess

parseCollapseFlag :: String -> Maybe (Maybe Int)
parseCollapseFlag ('-':'C':rest) = 
  case rest of
    "" -> Just Nothing
    n  -> Just (readMaybe n)
parseCollapseFlag _ = Nothing

printHelp :: IO (Either String ())
printHelp = do
  putStrLn "HVM usage:"
  putStrLn "  hvm3 help       # Shows this help message"
  putStrLn "  hvm3 run <file> [flags] [args...] # Evals main"
  putStrLn "  hvm3 serve <file> [flags] # Starts socket server on port 8080"
  putStrLn "    -c  # Runs with compiled mode (fast)"
  putStrLn "    -C  # Collapse the result to a list of λ-Terms"
  putStrLn "    -CN # Same as above, but show only first N results"
  putStrLn "    -s  # Show statistics"
  putStrLn "    -d  # Print execution steps (debug mode)"
  putStrLn "    -Q  # Hide quotes in output"
  putStrLn "    -h  # Generate memory access heatmap to heatmap.png"
  return $ Right ()

-- CLI Commands
-- ------------

cliRun :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> Bool -> [String] -> IO (Either String ())
cliRun filePath debug compiled mode showStats hideQuotes heatmap strArgs = do
  code <- readFile' filePath
  book <- doParseBook filePath code
  if not heatmap
    then do
      hvmInit
      initBook filePath book compiled
      checkHasMain book
      args <- doParseArguments book strArgs
      checkMainArgs book args
      (_, stats) <- withRunStats $ do
        injectRoot book (Ref "main" maxBound args)
        rxAt <- if compiled then return (reduceCAt debug) else return (reduceAt debug)
        case mode of
          Collapse limit -> do
            core <- doCollapseFlatAt rxAt book 0
            let vals = maybe id Prelude.take limit core
            forM_ vals $ \val -> do
              let out = if hideQuotes then removeQuotes (show val) else show val
              printf "%s\n" out
          Normalize -> do
            core <- doExtractCoreAt rxAt book 0
            let val = doLiftDups core
            let out = if hideQuotes then removeQuotes (show val) else show val
            printf "%s\n" out
      hvmFree
      when showStats $ print stats
      return $ Right ()
    else do
      -- Dry run to obtain time and max memory size
      hvmInit
      initBook filePath book compiled
      checkHasMain book
      args <- doParseArguments book strArgs
      checkMainArgs book args
      (vals1, stats1) <- runBook book (Ref "main" maxBound args) mode compiled debug
      -- Discard outputs from dry run; free and re-init
      hvmFree
      -- Second run with heatmap enabled
      hvmInit
      initBook filePath book compiled
      injectRoot book (Ref "main" maxBound args)
      heatmapBegin (rsTime stats1) (rsSize stats1) (rsItrs stats1) 1280 1280
      rxAt <- if compiled then return (reduceCAt debug) else return (reduceAt debug)
      ((), stats2) <- withRunStats $ do
        case mode of
          Collapse limit -> do
            core <- doCollapseFlatAt rxAt book 0
            let vals = maybe id Prelude.take limit core
            forM_ vals $ \val -> do
              let out = if hideQuotes then removeQuotes (show val) else show val
              printf "%s\n" out
          Normalize -> do
            core <- doExtractCoreAt rxAt book 0
            let val = doLiftDups core
            let out = if hideQuotes then removeQuotes (show val) else show val
            printf "%s\n" out
        return ()
      when showStats $ print stats2
      -- Render the heatmap.png
      renderHeatmapPNG "heatmap.png"
      heatmapEnd
      hvmFree
      return $ Right ()

-- Render heatmap using JuicyPixels from the runtime counters
renderHeatmapPNG :: FilePath -> IO ()
renderHeatmapPNG outPath = do
  let toInt = fromIntegral :: Word32 -> Int
  w32 <- heatmapGetWidth
  h32 <- heatmapGetHeight
  let w = fromIntegral w32 :: Int
  let h = fromIntegral h32 :: Int
  readsPtr  <- heatmapGetReads
  writesPtr <- heatmapGetWrites
  -- Copy arrays to Haskell
  let len = w * h
  readsL  <- peekArray len readsPtr
  writesL <- peekArray len writesPtr
  let readsV  = VU.fromListN len readsL  :: VU.Vector Word32
  let writesV = VU.fromListN len writesL :: VU.Vector Word32
  -- Find global max
  let totals = [ fromIntegral (readsV VU.! i) + fromIntegral (writesV VU.! i) | i <- [0 .. len - 1] ] :: [Double]
  let maxTot = maximum (0.0 : totals)
  let scale c = if maxTot <= 0 then 0 else realToFrac (log (1 + c) / log (1 + maxTot)) :: Double
  let toPix i =
        let r  = fromIntegral (readsV  VU.! i) :: Double
            wv = fromIntegral (writesV VU.! i) :: Double
            t = r + wv
        in if t <= 0
             then (255,255,255)
             else let d = scale t
                      p = r / t -- read proportion
                      base | p >= 0.5 = -- between gray and red
                               let a = (p - 0.5) * 2 in -- 0..1
                               let r0 = round (90   * (1 - a) + 160 * a) :: Int
                                   g0 = round (90   * (1 - a) +   0 * a) :: Int
                                   b0 = round (90   * (1 - a) +   0 * a) :: Int
                               in (r0,g0,b0)
                          | otherwise = -- between green and gray
                            let a = (0.5 - p) * 2 in -- 0..1
                            let r0 = round (90   * (1 - a) +   0 * a) :: Int
                                g0 = round (90   * (1 - a) + 160 * a) :: Int
                                b0 = round (90   * (1 - a) +   0 * a) :: Int
                            in (r0,g0,b0)
                      mix a b t' = round (fromIntegral a * d + fromIntegral b * (1 - d))
                      (br,bg,bb) = base
                  in ( mix br 255 d
                     , mix bg 255 d
                     , mix bb 255 d
                     )
  let img = generateImage
              (\x y -> let i = y * w + x
                           (r,g,b) = toPix i
                       in PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b))
              w h
  savePngImage outPath (ImageRGB8 img)

cliServe :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> IO (Either String ())
cliServe filePath debug compiled mode showStats hideQuotes = do
  code <- readFile' filePath
  book <- doParseBook filePath code
  hvmInit
  initBook filePath book compiled
  checkHasMain book
  putStrLn "HVM serve mode. Listening on port 8080."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  Network.bind sock (SockAddrInet 8080 0)
  listen sock 5
  putStrLn "Server started. Listening on port 8080."
  serverLoop sock book `finally` do
    close sock
    hvmFree
    putStrLn "\nServer terminated."
  return $ Right ()
  where
    serverLoop sock book = do
      result <- try $ do
        (conn, _) <- accept sock
        h <- socketToHandle conn ReadWriteMode
        hSetBuffering h LineBuffering
        hSetEncoding h utf8
        input <- hGetLine h
        unless (input == "exit" || input == "quit") $ do
          oldSize <- getLen
          args <- doParseArguments book [input]
          checkMainArgs book args
          let root = Ref "main" maxBound args
          (vals, stats) <- runBook book root mode compiled debug
          let out = unlines $ map (\t -> if hideQuotes then removeQuotes (show t) else show t) vals
          hPutStrLn h out
          when showStats $ do
            hPutStrLn h (show stats)
          setItr 0
          setLen oldSize
        hClose h
      case result of
        Left e -> case fromException e of
          Just UserInterrupt -> return () -- Exit loop on Ctrl+C
          _ -> do
            hPutStrLn stderr $ "Connection error: " ++ show (e :: SomeException)
            serverLoop sock book
        Right _ -> serverLoop sock book

removeQuotes :: String -> String
removeQuotes s = case s of
  '"':rest -> init rest  -- Remove first and last quote if present
  _        -> s          -- Otherwise return as-is

checkHasMain :: Book -> IO ()
checkHasMain book = do
  when (not $ MS.member "main" (namToFid book)) $ do
    putStrLn "Error: 'main' not found."
    exitWith (ExitFailure 1)

checkMainArgs :: Book -> [Core] -> IO ()
checkMainArgs book args = do
  let ((_, mainArgs), _) = mget (fidToFun book) (mget (namToFid book) "main")
  when (length args /= length mainArgs) $ do
    putStrLn $ "Error: 'main' expects " ++ show (length mainArgs) ++ " arguments, found " ++ show (length args)
    exitWith (ExitFailure 1)
