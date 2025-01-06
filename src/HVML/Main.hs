-- Type.hs:
-- //./Type.hs//

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, forM_)
import Data.FileEmbed
import Data.Time.Clock
import Data.Word
import Foreign.C.Types
import Foreign.LibFFI
import Foreign.LibFFI.Types
import GHC.Conc
import HVML.Collapse
import HVML.Compile
import HVML.Extract
import HVML.Inject
import HVML.Parse
import HVML.Reduce
import HVML.Show
import HVML.Type
import System.CPUTime
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO
import System.IO (readFile)
import System.Posix.DynamicLinker
import System.Process (callCommand)
import Text.Printf
import qualified Data.Map.Strict as MS

runtime_c :: String
runtime_c = $(embedStringFile "./src/HVML/Runtime.c")

-- Main
-- ----

data RunMode
  = Normalize
  | Collapse
  | Search
  deriving Eq

data InputType
  = FileInput FilePath
  | StringInput String
  deriving Eq

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    ("run" : input : args) -> do
      let compiled = "-c" `elem` args
      let collapse = "-C" `elem` args
      let search   = "-S" `elem` args
      let stats    = "-s" `elem` args
      let debug    = "-d" `elem` args
      let mode | collapse  = Collapse
               | search    = Search
               | otherwise = Normalize
      let inputType = if head input == '(' 
                     then StringInput input
                     else FileInput input
      cliRun inputType debug compiled mode stats
    ["help"] -> printHelp
    _ -> printHelp
  case result of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right _ -> do
      exitWith ExitSuccess

printHelp :: IO (Either String ())
printHelp = do
  putStrLn "HVM-Lazy usage:"
  putStrLn "  hvml help                  # Shows this help message"
  putStrLn "  hvml run <file>            # Evals main from file"
  putStrLn "  hvml run \"(expression)\"    # Evals expression directly"
  putStrLn "    -t # Returns the type (experimental)"
  putStrLn "    -c # Runs with compiled mode (fast)"
  putStrLn "    -C # Collapse the result to a list of λ-Terms"
  putStrLn "    -S # Search (collapse, then print the 1st λ-Term)"
  putStrLn "    -s # Show statistics"
  putStrLn "    -d # Print execution steps (debug mode)"
  return $ Right ()

-- CLI Commands
-- ------------

cliRun :: InputType -> Bool -> Bool -> RunMode -> Bool -> IO (Either String ())
cliRun input debug compiled mode showStats = do
  -- Initialize the HVM
  hvmInit
  
  -- Get the code either from file or wrap the string input
  code <- case input of
    FileInput path -> readFile path
    StringInput expr -> return $ wrapExpression expr
    
  book <- doParseBook code
  
  -- Create the C file content
  let funcs = map (\ (fid, _) -> compile book fid) (MS.toList (fidToFun book))
  let mainC = unlines $ [runtime_c] ++ funcs ++ [genMain book]
  
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
    -- Write the C file
    writeFile "./.main.c" mainC
    -- Compile to shared library
    callCommand "gcc -O2 -fPIC -shared .main.c -o .main.so"
    -- Load the dynamic library
    bookLib <- dlopen "./.main.so" [RTLD_NOW]
    -- Remove both generated files
    callCommand "rm .main.so"
    -- Register compiled functions
    forM_ (MS.keys (fidToFun book)) $ \ fid -> do
      funPtr <- dlsym bookLib (mget (fidToNam book) fid ++ "_f")
      hvmDefine fid funPtr
    -- Link compiled state
    hvmGotState <- hvmGetState
    hvmSetState <- dlsym bookLib "hvm_set_state"
    callFFI hvmSetState retVoid [argPtr hvmGotState]
  -- Abort when main isn't present
    
  -- Check for main
  when (not $ MS.member "main" (namToFid book)) $ do
    putStrLn "Error: 'main' not found."
    exitWith (ExitFailure 1)
    
  -- Normalize main
  init <- getCPUTime
  root <- doInjectCoreAt book (Ref "main" (mget (namToFid book) "main") []) 0 []
  rxAt <- if compiled
    then return (reduceCAt debug)
    else return (reduceAt debug)
  vals <- if mode == Collapse || mode == Search
    then doCollapseFlatAt rxAt book 0
    else do
      core <- doExtractCoreAt rxAt book 0
      return [(doLiftDups core)]
  -- Print all collapsed results
      
  -- Print results based on mode
  when (mode == Collapse) $ do
    forM_ vals $ \ term -> do
      putStrLn $ showCore term
  -- Prints just the first collapsed result
  when (mode == Search || mode == Normalize) $ do
    putStrLn $ showCore (head vals)
  when (mode /= Normalize) $ do
    putStrLn ""
  -- Prints total time
  end <- getCPUTime
  -- Show stats
  when showStats $ do
    itrs <- getItr
    size <- getLen
    let time = fromIntegral (end - init) / (10^12) :: Double
    let mips = (fromIntegral itrs / 1000000.0) / time
    printf "WORK: %llu interactions\n" itrs
    printf "TIME: %.7f seconds\n" time
    printf "SIZE: %llu nodes\n" size
    printf "PERF: %.3f MIPS\n" mips
    return ()
  -- Finalize
  hvmFree
  return $ Right ()

wrapExpression :: String -> String
wrapExpression expr = unlines
  [ "@main = " ++ expr
  ]

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
    , "  printf(\"WORK: %llu interactions\\n\", get_itr());"
    , "  printf(\"TIME: %.3fs seconds\\n\", time / 1000.0);"
    , "  printf(\"SIZE: %u nodes\\n\", get_len());"
    , "  printf(\"PERF: %.3f MIPS\\n\", (get_itr() / 1000000.0) / (time / 1000.0));"
    , "  hvm_free();"
    , "  return 0;"
    , "}"
    ]
