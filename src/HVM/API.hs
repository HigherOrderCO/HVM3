module HVM.API where

import Control.DeepSeq (deepseq)
import Control.Monad (when, forM_, foldM)
import Data.Word (Word64)
import Foreign.LibFFI
import GHC.Clock
import HVM.Adjust
import HVM.Collapse
import HVM.Compile
import HVM.Extract
import HVM.Foreign
import HVM.Inject
import HVM.Parse
import HVM.Reduce
import HVM.Type
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO (readFile')
import System.IO.Error (tryIOError)
import System.Posix.DynamicLinker
import System.Process (callCommand)

import qualified Data.Map.Strict as MS

data RunMode
  = Normalize
  | Collapse (Maybe Int)
  deriving Eq

data RunStats = RunStats {
  rsItrs :: Word64,
  rsTime :: Double,
  rsSize :: Word64,
  rsPerf :: Double
}

runHVM :: FilePath -> [Core] -> RunMode -> IO ([Core], RunStats)
runHVM filePath args mode = do
  hvmInit
  book <- loadBook filePath True
  -- Need to adjust args since they were constructed without the book
  (vals, stats) <- runBook book (map (adjust book) args) mode True False
  hvmFree
  return (vals, stats)

runBook :: Book -> [Core] -> RunMode -> Bool -> Bool -> IO ([Core], RunStats)
runBook book args mode compiled debug = do
  init <- getMonotonicTimeNSec
  root <- injectArgs book args
  rxAt <- if compiled
    then return (reduceCAt debug)
    else return (reduceAt debug)
  vals <- case mode of
    Collapse limit -> do
      core <- doCollapseFlatAt rxAt book 0
      let vals = maybe id Prelude.take limit core
      vals `deepseq` return vals
    Normalize -> do
      core <- doExtractCoreAt rxAt book 0
      let vals = [doLiftDups core]
      vals `deepseq` return vals
  end <- getMonotonicTimeNSec
  itrs <- getItr
  size <- getLen
  let time = fromIntegral (end - init) / (10^9) :: Double
  let mips = (fromIntegral itrs / 1000000.0) / time
  let stats = RunStats { rsItrs = itrs, rsSize = size , rsTime = time, rsPerf = mips }
  return (vals, stats)

-- Load and initialize the runtime with a book from a file
loadBook :: FilePath -> Bool -> IO Book
loadBook filePath compiled = do
  code <- readFile' filePath
  book <- doParseBook filePath code
  -- Abort when main isn't present
  when (not $ MS.member "main" (namToFid book)) $ do
    putStrLn "Error: 'main' not found."
    exitWith (ExitFailure 1)

  forM_ (MS.toList (cidToAri book)) $ \(cid, ari) -> hvmSetCari cid (fromIntegral ari)
  forM_ (MS.toList (cidToLen book)) $ \(cid, len) -> hvmSetClen cid (fromIntegral len)
  forM_ (MS.toList (cidToADT book)) $ \(cid, adt) -> hvmSetCadt cid (fromIntegral adt)
  forM_ (MS.toList (fidToFun book)) $ \(fid, ((_, args), _)) -> hvmSetFari fid (fromIntegral $ length args)

  when compiled $ do
    let mainC = compileBook book runtime_c
    callCommand "mkdir -p .build"
    let fName = last $ words $ map (\c -> if c == '/' then ' ' else c) filePath
    let cPath = ".build/" ++ fName ++ ".c"
    let oPath = ".build/" ++ fName ++ ".so"
    oldCFile <- tryIOError (readFile' cPath)
    bookLib <- if oldCFile == Right mainC then dlopen oPath [RTLD_NOW]
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
  return book

-- Parse arguments and create a term with the given function name
injectArgs :: Book -> [Core] -> IO Term
injectArgs book args = do
  -- Abort when wrong number of args
  let ((_, mainArgs), _) = mget (fidToFun book) (mget (namToFid book) "main")
  when (length args /= length mainArgs) $ do
    putStrLn $ "Error: 'main' expects " ++ show (length mainArgs)
              ++ " arguments, found " ++ show (length args)
    exitWith (ExitFailure 1)
  let fid = mget (namToFid book) "main"
  let main = Ref "main" fid args
  root <- doInjectCoreAt book main 0 []
  return root
