module HVM.API where

import Control.DeepSeq (deepseq)
import Control.Monad (when, forM_)
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
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (readFile')
import System.IO.Error (tryIOError)
import System.Posix.DynamicLinker
import System.Process (callCommand)
import Text.Printf

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

-- Main external API for running HVM
runHVM :: FilePath -> Core -> RunMode -> IO ([Core], RunStats)
runHVM filePath root mode = do
  code   <- readFile' filePath
  book   <- doParseBook filePath code
  hvmInit
  initBook filePath book True
  (vals, stats) <- runBook book root mode True False
  hvmFree
  return (vals, stats)

-- Initializes the runtime with the definitions from a book
initBook :: FilePath -> Book -> Bool -> IO ()
initBook filePath book compiled = do
  forM_ (MS.toList (cidToAri book)) $ \(cid, ari) -> hvmSetCari cid (fromIntegral ari)
  forM_ (MS.toList (cidToLen book)) $ \(cid, len) -> hvmSetClen cid (fromIntegral len)
  forM_ (MS.toList (cidToADT book)) $ \(cid, adt) -> hvmSetCadt cid (fromIntegral adt)
  forM_ (MS.toList (fidToFun book)) $ \(fid, ((_, args), _)) -> hvmSetFari fid (fromIntegral $ length args)
  when compiled $ do
    oPath <- compileBookToBin filePath book
    dylib <- dlopen oPath [RTLD_NOW]
    forM_ (MS.keys (fidToFun book)) $ \fid -> do
      funPtr <- dlsym dylib (mget (fidToNam book) fid ++ "_f")
      hvmDefine fid funPtr
    hvmGotState <- hvmGetState
    hvmSetState <- dlsym dylib "hvm_set_state"
    callFFI hvmSetState retVoid [argPtr hvmGotState]

runBook :: Book -> Core -> RunMode -> Bool -> Bool -> IO ([Core], RunStats)
runBook book root mode compiled debug =
  withRunStats $ do
    injectRoot book root
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
        let vals = [core]
        vals `deepseq` return vals
    return vals

compileBookToBin :: FilePath -> Book -> IO FilePath
compileBookToBin filePath book = do
  let mainC = compileBook book runtime_c
  callCommand "mkdir -p .build"
  let fName = last $ words $ map (\c -> if c == '/' then ' ' else c) filePath
  let cPath = ".build/" ++ fName ++ ".c"
  let oPath = ".build/" ++ fName ++ ".so"
  oldCFile <- tryIOError (readFile' cPath)
  when (oldCFile /= Right mainC) $ do
    writeFile cPath mainC
    callCommand $ "gcc -O2 -fPIC -shared " ++ cPath ++ " -o " ++ oPath
  return oPath

injectRoot :: Book -> Core -> IO ()
injectRoot book root = do
  let (book', root') = adjust "" book root []
  doInjectCoreAt book' root' 0 []
  return ()

withRunStats :: IO a -> IO (a, RunStats)
withRunStats action = do
  init <- getMonotonicTimeNSec
  res  <- action
  end  <- getMonotonicTimeNSec
  itrs <- getItr
  size <- getLen
  let time  = fromIntegral (end - init) / (10^9) :: Double
  let mips  = (fromIntegral itrs / 1000000.0) / time
  let stats = RunStats { rsItrs = itrs, rsSize = size , rsTime = time, rsPerf = mips }
  return (res, stats)

instance Show RunStats where
  show stats = printf "WORK: %llu interactions\n" (rsItrs stats) ++
               printf "TIME: %.7f seconds\n" (rsTime stats) ++
               printf "SIZE: %llu nodes\n" (rsSize stats) ++
               printf "PERF: %.3f MIPS\n" (rsPerf stats)
