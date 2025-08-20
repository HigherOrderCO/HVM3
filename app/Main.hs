module Main where

import Network.Socket as Network
import System.IO (hSetEncoding, utf8, hPutStrLn, stderr)
import Control.Exception (try, fromException, SomeException, finally, AsyncException(UserInterrupt))
import Control.Monad (when, forM_, unless, foldM)
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
import Text.Printf
import Text.Read (readMaybe)
import Data.Word
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
      let debug          = "-d" `elem` flags
      let hideQuotes     = "-Q" `elem` flags
      let interactions   = "-I" `elem` flags
      let mode           = case collapseFlag of { Just n -> Collapse n ; Nothing -> Normalize }
      cliRun file debug compiled mode stats hideQuotes interactions sArgs
    ("serve" : file : rest) -> do
      let (flags, _)     = partition ("-" `isPrefixOf`) rest
      let compiled       = "-c" `elem` flags
      let collapseFlag   = Data.List.find (isPrefixOf "-C") flags >>= parseCollapseFlag
      let stats          = "-s" `elem` flags
      let debug          = "-d" `elem` flags
      let hideQuotes     = "-Q" `elem` flags
      let mode           = case collapseFlag of { Just n -> Collapse n ; Nothing -> Normalize }
      cliServe file debug compiled mode stats hideQuotes
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
  putStrLn "    -I  # Show interaction statistics"
  putStrLn "    -d  # Print execution steps (debug mode)"
  putStrLn "    -Q  # Hide quotes in output"
  return $ Right ()

-- CLI Commands
-- ------------

cliRun :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> Bool -> [String] -> IO (Either String ())
cliRun filePath debug compiled mode showStats hideQuotes interactions strArgs = do
  code <- readFile' filePath
  book <- doParseBook filePath code
  hvmInit
  initBook filePath book compiled
  checkHasMain book
  args <- doParseArguments book strArgs
  checkMainArgs book args
  (_, stats) <- withRunStats $ do
    injectRoot book (Ref "main" maxBound args)
    rxAt <- if compiled
      then return (reduceCAt debug)
      else return (reduceAt debug)
    case mode of
      Collapse limit -> do
        core <- doCollapseFlatAt rxAt book 0
        let vals = maybe id Prelude.take limit core
        forM_ vals $ \val -> do -- Collapse and print the result line by line lazily
          let out = if hideQuotes then removeQuotes (show val) else show val
          printf "%s\n" out
      Normalize -> do
        core <- doExtractCoreAt rxAt book 0
        let val = doLiftDups core
        let out = if hideQuotes then removeQuotes (show val) else show val
        printf "%s\n" out
  when showStats $ do
    print stats
  when interactions $ do
    printInteractions book
  hvmFree
  return $ Right ()

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

-- | Prints interaction statistics with function names from the Book
printInteractions :: Book -> IO ()
printInteractions book = do
  -- Get interaction counts from HVM state
  let baseFns = 
        [ ("let_lazy", hvmGetLetLazy)
        , ("let_strict", hvmGetLetStri)
        , ("app_era", hvmGetAppEra)
        , ("app_lam", hvmGetAppLam)
        , ("app_sup", hvmGetAppSup)
        , ("dup_era", hvmGetDupEra)
        , ("dup_lam", hvmGetDupLam)
        , ("dup_sup_anni", hvmGetDupSupAnni)
        , ("dup_sup_comm", hvmGetDupSupComm)
        , ("dup_w32", hvmGetDupW32)
        , ("mat_era", hvmGetMatEra)
        , ("mat_sup", hvmGetMatSup)
        , ("opx_era", hvmGetOpxEra)
        , ("opx_sup", hvmGetOpxSup)
        , ("opx_w32", hvmGetOpxW32)
        , ("opy_era", hvmGetOpyEra)
        , ("opy_sup", hvmGetOpySup)
        , ("opy_w32", hvmGetOpyW32)
        , ("dup_ctr (total)", foldM (\acc i -> hvmGetDupCtr i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("mat_ctr (total)", foldM (\acc i -> hvmGetMatCtr i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_sup (total)", foldM (\acc i -> hvmGetRefSup i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_dup (total)", foldM (\acc i -> hvmGetRefDup i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_era (total)", foldM (\acc i -> hvmGetRefEra i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_fast (total)", foldM (\acc i -> hvmGetRefFast i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_slow (total)", foldM (\acc i -> hvmGetRefSlow i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_itrs (total)", foldM (\acc i -> hvmGetRefItrs i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_fall (total)", foldM (\acc i -> hvmGetRefFall i >>= \count -> return (acc + count)) 0 [0..65535])
        ]

  putStrLn "Interactions:"
  forM_ baseFns $ \(name, fn) -> do
    count <- fn
    printf "  %s: %s\n" name (formatLargeNumber count)
  
  putStrLn "Ctr interactions:"
  forM_ (MS.toList (cidToCtr book)) $ \(i, name) -> do
    dupCount <- hvmGetDupCtr i
    matCount <- hvmGetMatCtr i
    when (matCount > 0 || dupCount > 0) $ do
      printf "  %-15s: mat_ctr:%-12s dup_ctr:%-12s\n" name (formatLargeNumber matCount) (formatLargeNumber dupCount)  

  putStrLn "Call interactions:"
  forM_ (MS.toList (fidToFun book)) $ \(fid, (_, _)) -> do
    refFast <- hvmGetRefFast fid
    refSlow <- hvmGetRefSlow fid
    refItrs <- hvmGetRefItrs fid
    refFall <- hvmGetRefFall fid
    refEra <- hvmGetRefEra fid
    refDup <- hvmGetRefDup fid
    refSup <- hvmGetRefSup fid
    when (refFast > 0 || refSlow > 0) $ do
      (printf
        "  %-25s: fast:%-10s itrs:%-10s fall:%-10s sup:%-10s era:%-10s slow:%-10s\n"
        (mget (fidToNam book) fid)
        (formatLargeNumber refFast)
        (formatLargeNumber refItrs)
        (formatLargeNumber refFall)
        (formatLargeNumber refSup)
        (formatLargeNumber refEra)
        (formatLargeNumber refSlow))
  return ()

-- Helper to format large numbers
formatLargeNumber :: Word64 -> String
formatLargeNumber n
  | n < 1000000 = show n
  | otherwise     = printf "%.3f M" (fromIntegral n / 1000000.0 :: Double)

checkMainArgs :: Book -> [Core] -> IO ()
checkMainArgs book args = do
  let ((_, mainArgs), _) = mget (fidToFun book) (mget (namToFid book) "main")
  when (length args /= length mainArgs) $ do
    putStrLn $ "Error: 'main' expects " ++ show (length mainArgs) ++ " arguments, found " ++ show (length args)
    exitWith (ExitFailure 1)
