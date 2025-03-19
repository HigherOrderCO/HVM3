-- Type.hs:
-- //./Type.hs//

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (guard, when, forM_)
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
      let (flags,sArgs) = partition ("-" `isPrefixOf`) rest
      let compiled      = "-c" `elem` flags
      let collapseFlag  = Data.List.find (isPrefixOf "-C") flags >>= parseCollapseFlag
      let stats         = "-s" `elem` flags
      let debug         = "-d" `elem` flags
      let hideQuotes    = "-Q" `elem` flags
      let mode          = case collapseFlag of { Just n -> Collapse n ; Nothing -> Normalize }
      cliRun file debug compiled mode stats hideQuotes sArgs
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
  putStrLn "  hvm run <file> [flags] [string args...] # Evals main"
  putStrLn "    -t  # Returns the type (experimental)"
  putStrLn "    -c  # Runs with compiled mode (fast)"
  putStrLn "    -C  # Collapse the result to a list of λ-Terms"
  putStrLn "    -CN # Same as above, but show only first N results"
  putStrLn "    -s  # Show statistics"
  putStrLn "    -d  # Print execution steps (debug mode)"
  putStrLn "    -Q  # Hide quotes in output"
  return $ Right ()

-- CLI Commands
-- ------------

cliRun :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> [String] -> IO (Either String ())
cliRun filePath debug compiled mode showStats hideQuotes strArgs = do
  -- Initialize the HVM
  hvmInit
  code <- readFile' filePath
  book <- doParseBook filePath code
  -- Set constructor arities, case length and ADT ids
  forM_ (MS.toList (cidToAri book)) $ \ (cid, ari) -> do
    hvmSetCari cid (fromIntegral ari)
  forM_ (MS.toList (cidToLen book)) $ \ (cid, len) -> do
    hvmSetClen cid (fromIntegral len)
  forM_ (MS.toList (cidToADT book)) $ \ (cid, adt) -> do
    hvmSetCadt cid (fromIntegral adt)
  forM_ (MS.toList (fidToFun book)) $ \ (fid, ((_, args), _)) -> do
    hvmSetFari fid (fromIntegral $ length args)
  -- Compile to native
  when compiled $ do
    -- Create the C file content
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

    -- Register compiled functions
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
  -- Convert string arguments to Core terms and inject them at runtime
  let args = map (\str -> foldr (\c acc -> Ctr "#Cons" [Chr c, acc]) (Ctr "#Nil" []) str) strArgs
  root <- doInjectCoreAt book (Ref "main" (mget (namToFid book) "main") args) 0 []
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
    return ()
  -- Finalize
  hvmFree
  return $ Right ()

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

removeQuotes :: String -> String
removeQuotes s = case s of
  '"':rest -> init rest  -- Remove first and last quote if present
  _        -> s          -- Otherwise return as-is
