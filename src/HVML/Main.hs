-- Type.hs:
-- //./Type.hs//
-- Compile.hs:
-- //./Compile.hs//
-- Runtime.c:
-- //./Runtime.c//
-- Main.hs:

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, forM_)
import Data.FileEmbed
import Foreign.C.Types
import Foreign.LibFFI
import Foreign.LibFFI.Types
import System.CPUTime
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO
import System.IO (readFile)
import System.Posix.DynamicLinker
import System.Process (callCommand)
import qualified Data.Map.Strict as MS

import HVML.Compile
import HVML.Extract
import HVML.Inject
import HVML.Normal
import HVML.Parse
import HVML.Show
import HVML.Type

runtime_c :: String
runtime_c = $(embedStringFile "./src/HVML/Runtime.c")

-- Main
-- ----

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    ["run", file, "-s"] -> cliRun file True
    ["run", file]       -> cliRun file False
    ["help"]            -> printHelp
    _                   -> printHelp
  case result of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right _ -> do
      exitWith ExitSuccess

-- CLI Commands
-- ------------

cliRun :: FilePath -> Bool -> IO (Either String ())
cliRun filePath showStats = do
  -- Initialize the HVM
  hvmInit

  -- TASK: instead of parsing a core term out of the file, lets parse a Book.
  code <- readFile filePath
  let book = doParseBook code

  -- Create the C file content
  let funcs = map (\ (name, core) -> compile book name core) (MS.toList book)
  let bookC = unlines $ [runtime_c] ++ funcs
  let fids  = genBookIds book

  -- Write the C file
  writeFile "./.hvm_book.c" bookC
  
  -- Compile to shared library
  callCommand "gcc -O2 -fPIC -shared .hvm_book.c -o .hvm_book.so"
  
  -- Load the dynamic library
  bookLib <- dlopen "./.hvm_book.so" [RTLD_NOW]

  -- Remove both generated files
  callCommand "rm .hvm_book.c .hvm_book.so"

  -- Set the book's HVM pointers
  hvmGotState <- hvmGetState
  hvmSetState <- dlsym bookLib "hvm_set_state"
  callFFI hvmSetState retVoid [argPtr hvmGotState]
  
  -- For each function in the book, get its function pointer and register it
  forM_ (MS.keys book) $ \ name -> do
    funPtr <- dlsym bookLib ("F_" ++ name)
    hvmDefine (fromIntegral (MS.findWithDefault 0 name fids)) funPtr
    -- print ("defined " ++ name)

  -- Normalize main
  init <- getCPUTime
  root <- doInjectCore fids (Ref "main")
  norm <- normal root
  core <- doExtractCore norm
  putStrLn $ coreToString core

  -- Show stats
  when showStats $ do
    end <- getCPUTime
    itr <- getItr
    len <- getLen
    let diff = fromIntegral (end - init) / (10^9) :: Double
    let mips = (fromIntegral itr / 1000000.0) / (diff / 1000.0)
    putStrLn $ "ITRS: " ++ show itr
    putStrLn $ "TIME: " ++ show diff ++ "ms"
    putStrLn $ "SIZE: " ++ show len
    putStrLn $ "MIPS: " ++ show mips
    return ()

  -- Finalize
  hvmFree
  return $ Right ()

printHelp :: IO (Either String ())
printHelp = do
  putStrLn "HVM-Lazy usage:"
  putStrLn "  hvml run [-s] <file>  # Normalizes the specified file"
  putStrLn "  hvml help             # Shows this help message"
  return $ Right ()
