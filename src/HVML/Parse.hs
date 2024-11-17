-- //./Type.hs//
-- //./Show.hs//

module HVML.Parse where

import Data.List
import Data.Maybe
import Data.Word
import Debug.Trace
import HVML.Show
import HVML.Type
import Highlight (highlightError)
import System.Console.ANSI
import Text.Parsec hiding (State)
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.String
import qualified Data.Map.Strict as MS

-- Core Parsers
-- ------------

data ParserState = ParserState
  { parsedCtrToAri :: MS.Map String Int
  , parsedCtrToCid :: MS.Map String Word64
  }

type ParserM = Parsec String ParserState

parseCore :: ParserM Core
parseCore = do
  skip
  head <- lookAhead anyChar
  case head of
    '*' -> do
      consume "*"
      return Era
    'λ' -> do
      consume "λ"
      vr0 <- parseName
      bod <- parseCore
      return $ Lam vr0 bod
    '(' -> do
      next <- lookAhead (anyChar >> anyChar)
      case next of
        -- '@' -> parseRef
        '+' -> parseOper OP_ADD
        '-' -> parseOper OP_SUB
        '*' -> parseOper OP_MUL
        '/' -> parseOper OP_DIV
        '%' -> parseOper OP_MOD
        '=' -> parseOper OP_EQ
        '!' -> parseOper OP_NE
        '&' -> parseOper OP_AND
        '|' -> parseOper OP_OR
        '^' -> parseOper OP_XOR
        '<' -> do
          next <- lookAhead (anyChar >> anyChar >> anyChar)
          case next of
            '<' -> parseOper OP_LSH
            '=' -> parseOper OP_LTE
            _   -> parseOper OP_LT
        '>' -> do
          next <- lookAhead (anyChar >> anyChar >> anyChar)
          case next of
            '>' -> parseOper OP_RSH
            '=' -> parseOper OP_GTE
            _   -> parseOper OP_GT
        _ -> do
          consume "("
          fun <- parseCore
          args <- many $ do
            closeWith ")"
            parseCore
          char ')'
          return $ foldl App fun args
    '@' -> parseRef
    '&' -> do
      consume "&"
      lab <- read <$> many1 digit
      consume "{"
      tm0 <- parseCore
      tm1 <- parseCore
      consume "}"
      return $ Sup lab tm0 tm1
    '%' -> do
      consume "%"
      lab <- read <$> many1 digit
      consume "{"
      tm0 <- parseCore
      tm1 <- parseCore
      consume "}"
      return $ USp lab tm0 tm1
    '!' -> do
      consume "!"
      skip
      next <- lookAhead anyChar
      case next of
        '&' -> do
          -- parsing 'dup'
          consume "&"
          lab <- read <$> many1 digit
          consume "{"
          dp0 <- parseName
          dp1 <- parseName
          consume "}"
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ Dup lab dp0 dp1 val bod
        '%' -> do
          consume "%"
          lab <- read <$> many1 digit
          consume "{"
          dp0 <- parseName
          consume "}"
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ UDp lab dp0 val bod
        '!' -> do
          -- parsing strict 'let'
          consume "!"
          nam <- parseName
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ Let STRI nam val bod
        '^' -> do
          -- parsing parallel 'let'
          consume "^"
          nam <- parseName
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ Let PARA nam val bod
        _ -> do
          -- parsing lazy 'let'
          nam <- parseName
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ Let LAZY nam val bod
    '#' -> parseCtr
    '~' -> parseMat
    _ -> do
      name <- parseName
      case reads (filter (/= '_') name) of
        [(num, "")] -> return $ U32 (fromIntegral (num :: Integer))
        _           -> return $ Var name

parseRef :: ParserM Core
parseRef = do
  consume "@"
  name <- parseName
  args <- option [] $ do
    try $ string "("
    args <- many $ do
      closeWith ")"
      parseCore
    consume ")"
    return args
  return $ Ref name 0 args

parseCtr :: ParserM Core
parseCtr = do
  consume "#"
  nam <- parseName
  cid <- if length nam == 0
    then return 0
    else do
      cids <- parsedCtrToCid <$> getState
      case MS.lookup nam cids of
        Just id -> return id
        Nothing -> case reads nam of
          [(num, "")] -> return (fromIntegral (num :: Integer))
          otherwise   -> fail $ "Unknown constructor: " ++ nam
  fds <- option [] $ do
    try $ consume "{"
    fds <- many $ do
      closeWith "}"
      parseCore
    consume "}"
    return fds
  return $ Ctr cid fds

parseMat = do
  consume "~"
  val <- parseCore
  consume "{"
  css <- many $ do
    closeWith "}"
    next <- lookAhead anyChar
    case next of
      '#' -> do
        -- Parse ADT constructor case
        consume "#"
        name <- parseName
        cids <- parsedCtrToCid <$> getState
        aris <- parsedCtrToAri <$> getState
        ari  <- case MS.lookup name aris of
          Just ari -> return ari
          Nothing  -> return (-1)
        cid  <- case MS.lookup name cids of
          Just id -> return id
          Nothing -> if name == "_"
            then return 0xFFFFFFFF
            else fail $ "Unknown constructor: " ++ name
        consume ":"
        cas <- parseCore
        return (cid, (ari, cas))
      _ -> do
        -- Parse U32 or wildcard case
        name <- parseName
        cid <- if name == "_"
          then return 0xFFFFFFFF
          else case reads name of
            [(num, "")] -> return (fromIntegral (num :: Integer))
            _ -> fail $ "Invalid numeric pattern or wildcard"
        consume ":"
        cas <- parseCore
        return (cid, (-1, cas))
  consume "}"
  let sortedCss = map snd $ sortOn fst css
  return $ Mat val sortedCss

parseOper :: Oper -> ParserM Core
parseOper op = do
  consume "("
  consume (operToString op)
  nm0 <- parseCore
  nm1 <- parseCore
  consume ")"
  return $ Op2 op nm0 nm1

parseName :: ParserM String
parseName = skip >> many (alphaNum <|> char '_')

parseName1 :: ParserM String
parseName1 = skip >> many1 (alphaNum <|> char '_')

-- parseDef :: ParserM (String, ([String], Core))
-- parseDef = do
  -- try $ do
    -- skip
    -- consume "@"
  -- name <- parseName
  -- args <- many $ do
    -- closeWith "="
    -- name <- parseName1
    -- return name
  -- skip
  -- consume "="
  -- core <- parseCore
  -- trace ("PARSED: " ++ name ++ " " ++ show args ++ " = " ++ coreToString core) $ return ()
  -- return (name, (args, core))

-- TODO: update the syntax of definitions, from '@foo x y z = body' to '@foo(x,y,z) = body'. update the trace too
parseDef :: ParserM (String, ([String], Core))
parseDef = do
  try $ do
    skip
    consume "@"
  name <- parseName
  args <- option [] $ do
    try $ string "("
    args <- many $ do
      closeWith ")"
      parseName
    consume ")"
    return args
  skip
  consume "="
  core <- parseCore
  -- trace ("PARSED: " ++ name ++ "(" ++ intercalate "," args ++ ") = " ++ coreToString core) $ return ()
  return (name, (args, core))

parseADT :: ParserM ()
parseADT = do
  try $ do
    skip
    consume "data"
  name <- parseName
  skip
  consume "{"
  constructors <- many parseADTCtr
  consume "}"
  let ctrCids = zip (map fst constructors) [0..]
  let ctrAris = zip (map fst constructors) (map (fromIntegral . length . snd) constructors)
  modifyState (\s -> s { parsedCtrToCid = MS.union (MS.fromList ctrCids) (parsedCtrToCid s),
                         parsedCtrToAri = MS.union (MS.fromList ctrAris) (parsedCtrToAri s) })

parseADTCtr :: ParserM (String, [String])
parseADTCtr = do
  skip
  consume "#"
  name <- parseName
  fields <- option [] $ do
    try $ consume "{"
    fds <- many $ do
      closeWith "}"
      parseName
    skip
    consume "}"
    return fds
  skip
  return (name, fields)

parseBook :: ParserM [(String, ([String], Core))]
parseBook = do
  skip
  many parseADT
  defs <- many parseDef
  skip
  eof
  return defs

doParseCore :: String -> IO Core
doParseCore code = case runParser parseCore (ParserState MS.empty MS.empty) "" code of
  Right core -> return $ core
  Left  err  -> do
    showParseError "" code err
    return $ Ref "⊥" 0 []

doParseBook :: String -> IO Book
doParseBook code = case runParser parseBookWithState (ParserState MS.empty MS.empty) "" code of
  Right (defs, st) -> do
    return $ createBook defs (parsedCtrToCid st) (parsedCtrToAri st)
  Left err -> do
    showParseError "" code err
    return $ Book MS.empty MS.empty MS.empty MS.empty MS.empty
  where
    parseBookWithState :: ParserM ([(String, ([String], Core))], ParserState)
    parseBookWithState = do
      defs <- parseBook
      st   <- getState
      return (defs, st)


-- Helper Parsers
-- --------------

consume :: String -> ParserM String
consume str = spaces >> string str

closeWith :: String -> ParserM ()
closeWith str = try $ do
  spaces
  notFollowedBy (string str)

skip :: ParserM ()
skip = skipMany (parseSpace <|> parseComment) where
  parseSpace = (try $ do
    space
    return ()) <?> "space"
  parseComment = (try $ do
    string "//"
    skipMany (noneOf "\n")
    char '\n'
    return ()) <?> "Comment"

-- Adjusting
-- ---------

createBook :: [(String, ([String], Core))] -> MS.Map String Word64 -> MS.Map String Int -> Book
createBook defs ctrToCid ctrToAri =
  let nameToId' = MS.fromList $ zip (map fst defs) [0..]
      idToName' = MS.fromList $ map (\(k,v) -> (v,k)) $ MS.toList nameToId'
      idToFunc' = MS.fromList $ map (\ (name, (args, core)) -> (nameToId' MS.! name, (args, decorateFnIds nameToId' core))) defs
  in Book idToFunc' idToName' nameToId' ctrToAri ctrToCid

decorateFnIds :: MS.Map String Word64 -> Core -> Core
decorateFnIds fids term = case term of
  Var nam       -> Var nam
  Ref nam _ arg -> Ref nam (fids MS.! nam) (map (decorateFnIds fids) arg)
  Let m x v b   -> Let m x (decorateFnIds fids v) (decorateFnIds fids b)
  Lam x bod     -> Lam x (decorateFnIds fids bod)
  App f x       -> App (decorateFnIds fids f) (decorateFnIds fids x)
  Sup l x y     -> Sup l (decorateFnIds fids x) (decorateFnIds fids y)
  Dup l x y v b -> Dup l x y (decorateFnIds fids v) (decorateFnIds fids b)
  Ctr cid fds   -> Ctr cid (map (decorateFnIds fids) fds)
  Mat x css     -> Mat (decorateFnIds fids x) (map (\ (ar,cs) -> (ar, decorateFnIds fids cs)) css)
  Op2 op x y    -> Op2 op (decorateFnIds fids x) (decorateFnIds fids y)
  U32 n         -> U32 n
  Era           -> Era
  USp l x y     -> USp l (decorateFnIds fids x) (decorateFnIds fids y)
  UDp l x v b   -> UDp l x (decorateFnIds fids v) (decorateFnIds fids b)

-- Errors
-- ------

-- Error handling
extractExpectedTokens :: ParseError -> String
extractExpectedTokens err =
    let expectedMsgs = [msg | Expect msg <- errorMessages err, msg /= "space", msg /= "Comment"]
    in intercalate " | " expectedMsgs

showParseError :: String -> String -> ParseError -> IO ()
showParseError filename input err = do
  let pos = errorPos err
  let lin = sourceLine pos
  let col = sourceColumn pos
  let errorMsg = extractExpectedTokens err
  putStrLn $ setSGRCode [SetConsoleIntensity BoldIntensity] ++ "\nPARSE_ERROR" ++ setSGRCode [Reset]
  putStrLn $ "- expected: " ++ errorMsg
  putStrLn $ "- detected:"
  putStrLn $ highlightError (lin, col) (lin, col + 1) input
  putStrLn $ setSGRCode [SetUnderlining SingleUnderline] ++ filename ++ setSGRCode [Reset]
