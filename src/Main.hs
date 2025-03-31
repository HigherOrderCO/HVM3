-- Type.hs:
-- //./Type.hs//

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (guard, when, forM_, unless, foldM)
import Control.Exception (catch, SomeException)
import Data.FileEmbed
import Data.List (partition, isPrefixOf, take, find)
import Data.Word
import Data.Time (UTCTime)
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
import Text.Parsec (runParserT)
import System.IO.Unsafe (unsafePerformIO)
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
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, getCurrentDirectory)

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
  hvmInit
  createDirectoryIfMissing True ".build"
  
  code <- readFile' filePath
  book <- doParseBook filePath code
  
  -- Extract cached import paths from the parser state
  parserStateResult <- runParserT parseBookWithState (ParserState MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty 0) "" code
  let cachedPaths = case parserStateResult of
        Right (_, st) -> MS.keys (cachedImported st)
        Left _        -> []
        
  -- Split book into cached and non-cached parts
  let (cachedDefs, localDefs) = partition (\(name, _) -> any (\path -> isPrefixOf path name) cachedPaths) (MS.toList (namToFid book))
  let cachedBook = createSubBook book cachedDefs
  let localBook = createSubBook book localDefs
  
  -- Compile cached imports if necessary
  when compiled $ do
    forM_ cachedPaths $ \cachedPath -> do
      let cachedFName = last $ words $ map (\c -> if c == '/' then ' ' else c) cachedPath
      let cachedCPath = ".build/" ++ cachedFName ++ ".c"
      let cachedOPath = ".build/" ++ cachedFName ++ ".so"
      let cachedBookSubset = filterBookByPath cachedBook cachedPath
      
      -- Check if we need to recompile by comparing the file modification time
      needRecompile <- checkNeedRecompile cachedPath cachedOPath
      
      cachedLib <- if needRecompile
        then do
          -- Recompile if needed
          let cContent = compileBookToC cachedBookSubset
          writeFile cachedCPath cContent
          callCommand $ "gcc -O2 -fPIC -shared " ++ cachedCPath ++ " -o " ++ cachedOPath
          dlopen cachedOPath [RTLD_NOW]
        else do
          -- Just load the existing shared object
          dlopen cachedOPath [RTLD_NOW]
      
      -- Register cached functions
      forM_ (MS.keys (fidToFun cachedBookSubset)) $ \fid -> do
        funPtr <- dlsym cachedLib (mget (fidToNam cachedBookSubset) fid ++ "_f")
        hvmDefine fid funPtr
  
  -- Compile local book
  when compiled $ do
    let decls = compileHeaders localBook
    let funcs = map (\(fid, _) -> compile localBook fid) (MS.toList (fidToFun localBook))
    let mainC = unlines $ [runtime_c] ++ [decls] ++ funcs ++ [genMain localBook]
    let fName = last $ words $ map (\c -> if c == '/' then ' ' else c) filePath
    let cPath = ".build/" ++ fName ++ ".c"
    let oPath = ".build/" ++ fName ++ ".so"
    
    oldCFile <- tryIOError (readFile' cPath)
    bookLib <- if oldCFile == Right mainC
      then dlopen oPath [RTLD_NOW]
      else do
        writeFile cPath mainC
        callCommand $ "gcc -O2 -fPIC -shared " ++ cPath ++ " -o " ++ oPath
        dlopen oPath [RTLD_NOW]
    
    -- Register local functions
    forM_ (MS.keys (fidToFun localBook)) $ \fid -> do
      funPtr <- dlsym bookLib (mget (fidToNam localBook) fid ++ "_f")
      hvmDefine fid funPtr
    
    hvmGotState <- hvmGetState
    hvmSetState <- dlsym bookLib "hvm_set_state"
    callFFI hvmSetState retVoid [argPtr hvmGotState]
  
  -- Set up runtime state
  forM_ (MS.toList (cidToAri book)) $ \ (cid, ari) -> hvmSetCari cid (fromIntegral ari)
  forM_ (MS.toList (cidToLen book)) $ \ (cid, len) -> hvmSetClen cid (fromIntegral len)
  forM_ (MS.toList (cidToADT book)) $ \ (cid, adt) -> hvmSetCadt cid (fromIntegral adt)
  forM_ (MS.toList (fidToFun book)) $ \ (fid, ((_, args), _)) -> hvmSetFari fid (fromIntegral $ length args)
  
  -- Check for main in local file
  when (not $ MS.member "main" (namToFid localBook)) $ do
    putStrLn "Error: 'main' not found in local file."
    exitWith (ExitFailure 1)
    
  let ((_, mainArgs), _) = mget (fidToFun book) (mget (namToFid book) "main")
  when (length strArgs /= length mainArgs) $ do
    putStrLn $ "Error: 'main' expects " ++ show (length mainArgs)
               ++ " arguments, found " ++ show (length strArgs)
    exitWith (ExitFailure 1)
  
  init <- getMonotonicTimeNSec
  let args = map (\str -> foldr (\c acc -> Ctr "#Cons" [Chr c, acc]) (Ctr "#Nil" []) str) strArgs
  root <- doInjectCoreAt book (Ref "main" (mget (namToFid book) "main") args) 0 []
  rxAt <- if compiled then return (reduceCAt debug) else return (reduceAt debug)
  vals <- case mode of
    Collapse _ -> doCollapseFlatAt rxAt book 0
    Normalize -> do
      core <- doExtractCoreAt rxAt book 0
      return [(doLiftDups core)]
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
      let output = if hideQuotes then removeQuotes (showCore (head vals)) else showCore (head vals)
      putStrLn output
  end <- getMonotonicTimeNSec
  when showStats $ do
    itrs <- getItr
    size <- getLen
    let time = fromIntegral (end - init) / (10^9) :: Double
    let mips = (fromIntegral itrs / 1000000.0) / time
    printf "WORK: %llu interactions\n" itrs
    printf "TIME: %.7f seconds\n" time
    printf "SIZE: %llu nodes\n" size
    printf "PERF: %.3f MIPS\n" mips
  hvmFree
  return $ Right ()

-- Helper functions
checkNeedRecompile :: FilePath -> FilePath -> IO Bool
checkNeedRecompile sourcePath objectPath = do
  sourceExists <- doesFileExist sourcePath
  objectExists <- doesFileExist objectPath
  
  if not sourceExists then
    return True -- Source doesn't exist, this is an error condition
  else if not objectExists then
    return True -- Object doesn't exist, need to compile
  else do
    -- Compare modification times
    sourceTime <- getModificationTime sourcePath
    objectTime <- getModificationTime objectPath
    
    -- Check if there are any imports in the source that might be newer
    hasNewerImports <- checkImportsModTime sourcePath objectTime
    
    return (sourceTime > objectTime || hasNewerImports)

checkImportsModTime :: FilePath -> UTCTime -> IO Bool
checkImportsModTime filePath objTime = do
  result <- (do
    content <- readFile filePath
    imports <- extractImports content
    foldM (\acc imp -> do
              exists <- doesFileExist imp
              if exists then do
                impTime <- getModificationTime imp
                newerImports <- checkImportsModTime imp objTime
                return (acc || impTime > objTime || newerImports)
              else
                return acc
          ) False imports) `catch` \(_ :: SomeException) -> return False
  return result

extractImports :: String -> IO [FilePath]
extractImports content = do
  let contentLines = lines content
  return $ concatMap extractImportPath contentLines
  where
    extractImportPath line 
      | "import " `isPrefixOf` line = [drop 7 $ takeWhile (/= '\n') line]
      | "importCached " `isPrefixOf` line = [drop 13 $ takeWhile (/= '\n') line]
      | otherwise = []

createSubBook :: Book -> [(String, Word16)] -> Book
createSubBook fullBook defs = 
  let namToFid' = MS.fromList defs
      fidToNam' = MS.fromList (map (\(n, f) -> (f, n)) defs)
      fidToFun' = MS.filterWithKey (\k _ -> k `elem` map snd defs) (fidToFun fullBook)
      fidToLab' = MS.filterWithKey (\k _ -> k `elem` map snd defs) (fidToLab fullBook)
  in Book
       { fidToFun = fidToFun'
       , fidToNam = fidToNam'
       , fidToLab = fidToLab'
       , namToFid = namToFid'
       , cidToAri = cidToAri fullBook
       , cidToCtr = cidToCtr fullBook
       , ctrToCid = ctrToCid fullBook
       , cidToLen = cidToLen fullBook
       , cidToADT = cidToADT fullBook
       }

filterBookByPath :: Book -> String -> Book
filterBookByPath book path =
  let defs = filter (\(name, _) -> isPrefixOf path name) (MS.toList (namToFid book))
  in createSubBook book defs

compileBookToC :: Book -> String
compileBookToC book =
  let decls = compileHeaders book
      funcs = map (\(fid, _) -> compile book fid) (MS.toList (fidToFun book))
  in unlines $ [runtime_c] ++ [decls] ++ funcs

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