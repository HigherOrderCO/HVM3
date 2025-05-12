module Main where

import Network.Socket as Network
import System.IO (hSetEncoding, utf8, hPutStrLn, stderr)
import Control.Exception (try, fromException, SomeException, finally, AsyncException(UserInterrupt))
import Control.Monad (when, forM_, unless)
import Data.List (partition, isPrefixOf, find)
import HVM.API
import HVM.Foreign
import HVM.Parse
import HVM.Type
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO
import Text.Printf
import Text.Read (readMaybe)

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
  putStrLn "  hvm3 help       # Shows this help message"
  putStrLn "  hvm3 run <file> [flags] [args...] # Evals main"
  putStrLn "  hvm3 serve <file> [flags] # Starts socket server on port 8080"
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
  book <- loadBook filePath compiled
  args <- doParseArguments book strArgs
  (vals, stats) <- runBook book args mode compiled debug
  hvmFree
  forM_ vals $ \term -> do
    let output = if hideQuotes then removeQuotes (show term) else show term
    printf "%s\n" output
  when showStats $ do
    printf "WORK: %llu interactions\n" (rsItrs stats)
    printf "TIME: %.7f seconds\n" (rsTime stats)
    printf "SIZE: %llu nodes\n" (rsSize stats)
    printf "PERF: %.3f MIPS\n" (rsPerf stats)
    return ()
  return $ Right ()

cliServe :: FilePath -> Bool -> Bool -> RunMode -> Bool -> Bool -> IO (Either String ())
cliServe filePath debug compiled mode showStats hideQuotes = do
  hvmInit
  book <- loadBook filePath compiled
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
          (vals, stats) <- runBook book args mode compiled debug
          let output = unlines $ map (\t -> if hideQuotes then removeQuotes (show t) else show t) vals
          hPutStrLn h output
          when showStats $ do
            hPutStrLn h $ "WORK: " ++ show (rsItrs stats) ++ " interactions"
            hPutStrLn h $ "TIME: " ++ printf "%.7f" (rsTime stats) ++ " seconds"
            hPutStrLn h $ "SIZE: " ++ show (rsSize stats) ++ " nodes"
            hPutStrLn h $ "PERF: " ++ printf "%.3f" (rsPerf stats) ++ " MIPS"
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