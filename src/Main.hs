-- Type.hs:
-- //./Type.hs//

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Socket as Network
import System.IO (hSetEncoding, utf8)
import Control.Monad (guard, when, forM_, foldM, unless)
import Data.FileEmbed
import Data.List (partition, isPrefixOf, take, find)
import Data.Word
import Foreign.C.Types
import Foreign.LibFFI
import Foreign.LibFFI.Types
import GHC.Clock
import GHC.Conc
import Collapse
import Compile
import Extract
import Foreign
import Inject
import Parse
import Reduce
import Show
import Type
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO
import System.IO (readFile')
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error (tryIOError)
import System.Posix.DynamicLinker
import System.Process (callCommand)
import Text.Printf
import Data.IORef
import qualified Data.Map.Strict as MS
import Text.Read (readMaybe)

runtime_c :: String
runtime_c = $(embedStringFile "./src/Runtime.c")

-- Main
-- ----

data RunMode
  = Normalize
  | Collapse (Maybe Int)
  deriving Eq

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
      let mode           = case collapseFlag of { Just n -> Collapse n ; Nothing -> Normalize }
      cliRun file debug compiled mode stats hideQuotes sArgs
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
  putStrLn "  hvm help       # Shows this help message"
  putStrLn "  hvm run <file> [flags] [args...] # Evals main"
  putStrLn "  hvm serve <file> [flags] # Starts socket server on port 8080"
  putStrLn "    -c  # Runs with compiled mode (fast)"
  putStrLn "    -C  # Collapse the result to a list of λ-Terms"
  putStrLn "    -CN # Same as above, but show only first N results"
  putStrLn "    -s  # Show statistics"
  putStrLn "    -d  # Print execution steps (debug mode)"
  putStrLn "    -Q  # Hide quotes in output"
  return $ Right ()

-- CLI Commands
-- ------------

cliServe :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> IO (Either String ())
cliServe filePath debug compiled mode showStats hideQuotes = do
  -- Initialize the HVM
  hvmInit
  code <- readFile' filePath
  book <- doParseBook filePath code
  -- Set constructor arities, case lengths, and ADT IDs
  forM_ (MS.toList (cidToAri book)) $ \(cid, ari) -> do
    hvmSetCari cid (fromIntegral ari)
  forM_ (MS.toList (cidToLen book)) $ \(cid, len) -> do
    hvmSetClen cid (fromIntegral len)
  forM_ (MS.toList (cidToADT book)) $ \(cid, adt) -> do
    hvmSetCadt cid (fromIntegral adt)
  forM_ (MS.toList (fidToFun book)) $ \(fid, ((_, args), _)) -> do
    hvmSetFari fid (fromIntegral $ length args)

  when compiled $ do
    let decls = compileHeaders book
    let funcs = map (\(fid, _) -> compile book fid) (MS.toList (fidToFun book))
    let mainC = unlines [runtime_c, decls] ++ unlines funcs
    callCommand "mkdir -p .build"
    let fName = last $ words $ map (\c -> if c == '/' then ' ' else c) filePath
    let cPath = ".build/" ++ fName ++ ".c"
    let oPath = ".build/" ++ fName ++ ".so"
    oldCFile <- tryIOError (readFile' cPath)
    bookLib <- if oldCFile == Right mainC then do
      dlopen oPath [RTLD_NOW]
    else do
      writeFile cPath mainC
      callCommand $ "gcc -O2 -fPIC -shared " ++ cPath ++ " -o " ++ oPath
      dlopen oPath [RTLD_NOW]
    forM_ (MS.keys (fidToFun book)) $ \fid -> do
      funPtr <- dlsym bookLib (mget (fidToNam book) fid ++ "_f")
      hvmDefine fid funPtr
    hvmGotState <- hvmGetState
    hvmSetState <- dlsym bookLib "hvm_set_state"
    callFFI hvmSetState retVoid [argPtr hvmGotState]

    when (not $ MS.member "main" (namToFid book)) $ do
      putStrLn "Error: 'main' not found."
      exitWith (ExitFailure 1)

  serveSocket book debug compiled mode showStats hideQuotes

  hvmFree
  return $ Right ()

cliRun :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> [String] -> IO (Either String ())
cliRun filePath debug compiled mode showStats hideQuotes strArgs = do
  hvmInit
  code <- readFile' filePath
  book <- doParseBook filePath code

  forM_ (MS.toList (cidToAri book)) $ \ (cid, ari) -> do
    hvmSetCari cid (fromIntegral ari)
  forM_ (MS.toList (cidToLen book)) $ \ (cid, len) -> do
    hvmSetClen cid (fromIntegral len)
  forM_ (MS.toList (cidToADT book)) $ \ (cid, adt) -> do
    hvmSetCadt cid (fromIntegral adt)
  forM_ (MS.toList (fidToFun book)) $ \ (fid, ((_, args), _)) -> do
    hvmSetFari fid (fromIntegral $ length args)

  when compiled $ do
    let decls = compileHeaders book
    let funcs = map (\ (fid, _) -> compile book fid) (MS.toList (fidToFun book))
    let mainC = unlines $ [runtime_c] ++ [decls] ++ funcs ++ [genMain book]
    -- Try to use a cached .so file
    callCommand "mkdir -p .build"
    let fName = last $ words $ map (\c -> if c == '/' then ' ' else c) filePath
    let cPath = ".build/" ++ fName ++ ".c"
    let oPath = ".build/" ++ fName ++ ".so"

    oldCFile <- tryIOError (readFile' cPath)
    bookLib <- if oldCFile == Right mainC then do
      dlopen oPath [RTLD_NOW]
    else do
      writeFile cPath mainC
      callCommand $ "gcc -O2 -fPIC -shared " ++ cPath ++ " -o " ++ oPath
      dlopen oPath [RTLD_NOW]

    forM_ (MS.keys (fidToFun book)) $ \ fid -> do
      funPtr <- dlsym bookLib (mget (fidToNam book) fid ++ "_f")
      hvmDefine fid funPtr
    -- Link compiled state
    hvmGotState <- hvmGetState
    hvmSetState <- dlsym bookLib "hvm_set_state"
    callFFI hvmSetState retVoid [argPtr hvmGotState]
  -- Abort when main isn't present
  when (not $ MS.member "main" (namToFid book)) $ do
    putStrLn "Error: 'main' not found."
    exitWith (ExitFailure 1)
  -- Abort when wrong number of strArgs
  let ((_, mainArgs), _) = mget (fidToFun book) (mget (namToFid book) "main")
  when (length strArgs /= length mainArgs) $ do
    putStrLn $ "Error: 'main' expects " ++ show (length mainArgs)
              ++ " arguments, found " ++ show (length strArgs)
    exitWith (ExitFailure 1)
  -- Normalize main
  init <- getMonotonicTimeNSec

  -- Convert arguments to Core terms and inject them at runtime
  let args = map parseArgument strArgs
  let mainFid = mget (namToFid book) "main"
  let rawTerm = Ref "main" mainFid args
  let updatedTerm = setRefIds (namToFid book) rawTerm
  root <- doInjectCoreAt book updatedTerm 0 []
  rxAt <- if compiled
    then return (reduceCAt debug)
    else return (reduceAt debug)
  vals <- case mode of
    Collapse _ -> doCollapseFlatAt rxAt book 0
    Normalize -> do
      core <- doExtractCoreAt rxAt book 0
      return [(doLiftDups core)]
  -- Print results
  case mode of
    Collapse limit -> do
      lastItrs <- newIORef 0
      let limitedVals = maybe id Data.List.take limit vals
      forM_ limitedVals $ \ term -> do
        currItrs <- getItr
        prevItrs <- readIORef lastItrs
        let output = if hideQuotes then removeQuotes (showCore term) else showCore term
        printf "%s\n" output
        writeIORef lastItrs currItrs
      putStrLn ""
    Normalize -> do
      let output = if hideQuotes
                   then removeQuotes (showCore (head vals))
                   else showCore (head vals)
      putStrLn output
  -- Prints total time
  end <- getMonotonicTimeNSec
  -- Show stats
  when showStats $ do
    itrs <- getItr
    size <- getLen
    let time = fromIntegral (end - init) / (10^9) :: Double
    let mips = (fromIntegral itrs / 1000000.0) / time
    printf "WORK: %llu interactions\n" itrs
    printf "TIME: %.7f seconds\n" time
    printf "SIZE: %llu nodes\n" size
    printf "PERF: %.3f MIPS\n" mips
    printInteractions book
    return ()
  -- Finalize
  hvmFree
  return $ Right ()

parseArgument :: String -> Core
parseArgument str = unsafePerformIO $ doParseCore (str)

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
        , ("ref_f (total)", foldM (\acc i -> hvmGetRefF i >>= \count -> return (acc + count)) 0 [0..65535])
        , ("ref_t (total)", foldM (\acc i -> hvmGetRefT i >>= \count -> return (acc + count)) 0 [0..65535])
        ]

  putStrLn "Interactions:"
  forM_ baseFns $ \(name, fn) -> do
    count <- fn
    printf "  %s: %llu\n" name count
  
  putStrLn "Ctr interactions:"
  forM_ (MS.toList (cidToCtr book)) $ \(i, name) -> do
    dupCount <- hvmGetDupCtr i
    matCount <- hvmGetMatCtr i
    when (matCount > 0 || dupCount > 0) $ do
      printf "  %-15s: mat_ctr:%-12llu dup_ctr:%-12llu\n" name matCount dupCount  

  putStrLn "Call interactions:"
  forM_ (MS.toList (fidToFun book)) $ \(fid, (_, _)) -> do
    callF <- hvmGetRefF fid
    callT <- hvmGetRefT fid
    refEra <- hvmGetRefEra fid
    refDup <- hvmGetRefDup fid
    refSup <- hvmGetRefSup fid
    when (callF > 0) $ do
      printf "  %-15s: call_f:%-10llu call_t:%-10llu dup:%-10llu sup:%-10llu era:%-10llu\n" (mget (fidToNam book) fid) callF callT refDup refSup refEra

genMain :: Book -> String
genMain book =
  let mainFid = mget (namToFid book) "main"
      registerFuncs = unlines ["  hvm_define(" ++ show fid ++ ", " ++ mget (fidToNam book) fid ++ "_f);" | fid <- MS.keys (fidToFun book)]
  in unlines
    [ "int main() {"
    , "  hvm_init();"
    , registerFuncs
    , "  clock_t start = clock();"
    , "  Term root = term_new(REF, "++show mainFid++", 0);"
    , "  normal(root);"
    , "  double time = (double)(clock() - start) / CLOCKS_PER_SEC * 1000;"
    , "  printf(\"WORK: %\"PRIu64\" interactions\\n\", get_itr());"
    , "  printf(\"TIME: %.3fs seconds\\n\", time / 1000.0);"
    , "  printf(\"SIZE: %llu nodes\\n\", get_len());"
    , "  printf(\"PERF: %.3f MIPS\\n\", (get_itr() / 1000000.0) / (time / 1000.0));"
    , "  hvm_free();"
    , "  return 0;"
    , "}"
    ]

serveSocket :: Book -> Bool -> Bool -> RunMode -> Bool -> Bool -> IO ()
serveSocket book debug compiled mode showStats hideQuotes = do
  putStrLn "HVM serve mode. Listening on port 8080."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  Network.bind sock (SockAddrInet 8080 0)
  listen sock 5
  loop sock
  where
    loop sock = do
      (conn, _) <- accept sock
      h <- socketToHandle conn ReadWriteMode
      hSetBuffering h LineBuffering
      hSetEncoding h utf8 
      input <- hGetLine h
      unless (input == "exit" || input == "quit") $ do
        term <- doParseCore input
        let updatedTerm = setRefIds (namToFid book) term
        oldSize <- getLen
        let mainFid = mget (namToFid book) "main"
        root <- doInjectCoreAt book (Ref "main" mainFid [updatedTerm]) 0 []
        rxAt <- if compiled then return (reduceCAt debug) else return (reduceAt debug)
        vals <- case mode of
          Collapse _ -> doCollapseFlatAt rxAt book 0
          Normalize -> do
            core <- doExtractCoreAt rxAt book 0
            return [doLiftDups core]
        let output = case mode of
              Collapse limit -> do
                let limitedVals = maybe id Data.List.take limit vals
                let outputs = map (\term -> if hideQuotes then removeQuotes (showCore term) else showCore term) limitedVals
                unlines outputs
              Normalize -> do
                let result = head vals
                if hideQuotes then removeQuotes (showCore result) else showCore result
        hPutStrLn h output
        setLen oldSize
        when showStats $ do
          itrs <- getItr
          size <- getLen
          hPutStrLn h $ "WORK: " ++ (show itrs) ++ " interactions"
          hPutStrLn h $ "SIZE: " ++ (show size) ++ " nodes"
          setItr 0
      hClose h
      loop sock

removeQuotes :: String -> String
removeQuotes s = case s of
  '"':rest -> init rest  -- Remove first and last quote if present
  _        -> s          -- Otherwise return as-is
