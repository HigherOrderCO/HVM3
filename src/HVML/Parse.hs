-- //./Type.hs//

module HVML.Parse where

import Control.Monad (foldM, forM)
import Control.Monad.State
import Data.Either (isLeft)
import Data.List
import Data.Maybe
import Data.Word
import Debug.Trace
import HVML.Show
import HVML.Type
import Highlight (highlightError)
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
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
  , freshLabel     :: Word64
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
      vr0 <- parseName1
      bod <- parseCore
      return $ Lam vr0 bod
    '(' -> do
      next <- lookAhead (anyChar >> anyChar)
      case next of
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
    '@' -> do
      parseRef
    '&' -> do
      consume "&"
      name <- parseName
      next <- optionMaybe $ try $ char '{'
      case next of
        Just _ -> do
          tm0 <- parseCore
          tm1 <- parseCore
          consume "}"
          if null name then do
            num <- genFreshLabel
            return $ Sup num tm0 tm1
          else case reads name of
            [(num :: Word64, "")] -> do
              return $ Sup num tm0 tm1
            otherwise -> do
              return $ Ref "SUP" _SUP_F_ [Var ("&" ++ name), tm0, tm1]
        Nothing -> do
          return $ Var ("&" ++ name)
    '!' -> do
      consume "!"
      skip
      next <- lookAhead anyChar
      case next of
        '&' -> do
          consume "&"
          nam <- parseName
          consume "{"
          dp0 <- parseName1
          dp1 <- parseName1
          consume "}"
          consume "="
          val <- parseCore
          bod <- parseCore
          if null nam then do
            num <- genFreshLabel
            return $ Dup num dp0 dp1 val bod
          else case reads nam of
            [(num :: Word64, "")] -> do
              return $ Dup num dp0 dp1 val bod
            otherwise -> do
              return $ Ref "DUP" _DUP_F_ [Var ("&" ++ nam), val, Lam dp0 (Lam dp1 bod)]
        '!' -> do
          consume "!"
          nam <- optionMaybe $ try $ do
            nam <- parseName1
            consume "="
            return nam
          val <- parseCore
          bod <- parseCore
          case nam of
            Just nam -> return $ Let STRI nam val bod
            Nothing  -> return $ Let STRI "_" val bod
        '^' -> do
          consume "^"
          nam <- parseName1
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ Let PARA nam val bod
        _ -> do
          nam <- parseName1
          consume "="
          val <- parseCore
          bod <- parseCore
          return $ Let LAZY nam val bod
    '#' -> parseCtr
    '~' -> parseMat
    '[' -> parseLst
    '\'' -> parseChr
    '"' -> parseStr
    _ -> do
      name <- parseName1
      case reads (filter (/= '_') name) of
        [(num, "")] -> return $ U32 (fromIntegral (num :: Integer))
        _           -> return $ Var name

parseRef :: ParserM Core
parseRef = do
  consume "@"
  name <- parseName1
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
  nam <- parseName1
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

parseMat :: ParserM Core
parseMat = do
  consume "~"
  val <- parseCore
  -- Parse mov (external variables)
  mov <- many $ do
    try $ do
      skip
      consume "!"
    key <- parseName1
    val <- optionMaybe $ do
      try $ consume "="
      parseCore
    case val of
      Just v  -> return (key, v)
      Nothing -> return (key, Var key)
  consume "{"
  css <- many $ do
    closeWith "}"
    skip
    next <- lookAhead anyChar
    -- Parse constructor case
    if next == '#' then do
      consume "#"
      ctr <- parseName1
      fds <- option [] $ do
        try $ consume "{"
        fds <- many $ do
          closeWith "}"
          parseName1
        consume "}"
        return fds
      consume ":"
      bod <- parseCore
      return (ctr, fds, bod)
    -- Parse numeric or default case
    else do
      nam <- parseName1
      case reads nam of
        -- Numeric case
        [(n :: Word64, "")] -> do
          consume ":"
          bod <- parseCore
          return (nam, [], bod)
        -- Default case
        otherwise -> do
          consume ":"
          bod <- parseCore
          return ("_", [nam], bod)
  consume "}"
  css <- forM css $ \ (ctr, fds, bod) -> do
    cid <- case reads ctr of
      [(num, "")] -> do
        return $ Left (read num :: Word64)
      otherwise -> do
        st <- getState
        return $ Right $ fromMaybe maxBound $ MS.lookup ctr (parsedCtrToCid st)
    return (cid, (ctr, fds, bod))
  css <- return $ map snd $ sortOn fst css

  -- Transform matches with default cases into nested chain of matches
  if length css == 1 && (let (ctr, _, _) = head css in ctr == "_") then do
    fail "Match with only a default case is not allowed."
  else if (let (ctr, _, _) = last css in ctr == "_") then do
    let defName = (let (_,[nm],_) = last css in nm)
    let ifLets  = intoIfLetChain (Var defName) mov (init css) defName (last css)
    return $ Let LAZY defName val ifLets
  else do
    return $ Mat val mov css

intoIfLetChain :: Core -> [(String, Core)] -> [(String, [String], Core)] -> String -> (String, [String], Core) -> Core
intoIfLetChain _ _ [] defName (_,_,defBody) = defBody
intoIfLetChain val mov ((ctr,fds,bod):css) defName defCase =
  let rest = intoIfLetChain val mov css defName defCase in 
  Mat val mov [(ctr, fds, bod), ("_", [defName], rest)]

parseOper :: Oper -> ParserM Core
parseOper op = do
  consume "("
  consume (operToString op)
  nm0 <- parseCore
  nm1 <- parseCore
  consume ")"
  return $ Op2 op nm0 nm1

parseEscapedChar :: ParserM Char
parseEscapedChar = choice
  [ try $ do
      char '\\'
      c <- oneOf "\\\"nrtbf0/\'"
      return $ case c of
        '\\' -> '\\'
        '/'  -> '/'
        '"'  -> '"'
        '\'' -> '\''
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'
        'b'  -> '\b'
        'f'  -> '\f'
        '0'  -> '\0'
  , try $ do
      string "\\u"
      code <- count 4 hexDigit
      return $ toEnum (read ("0x" ++ code) :: Int)
  , noneOf "\"\\"
  ]

parseChr :: ParserM Core
parseChr = do
  skip
  char '\''
  c <- parseEscapedChar
  char '\''
  return $ Chr c

parseStr :: ParserM Core
parseStr = do
  skip
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ foldr (\c acc -> Ctr 1 [Chr c, acc]) (Ctr 0 []) str

parseLst :: ParserM Core
parseLst = do
  skip
  char '['
  elems <- many $ do
    closeWith "]"
    parseCore
  char ']'
  return $ foldr (\x acc -> Ctr 1 [x, acc]) (Ctr 0 []) elems

parseName :: ParserM String
parseName = skip >> many (alphaNum <|> char '_' <|> char '$' <|> char '&')

parseName1 :: ParserM String
parseName1 = skip >> many1 (alphaNum <|> char '_' <|> char '$' <|> char '&')

parseDef :: ParserM (String, ((Bool, [(Bool, String)]), Core))
parseDef = do
  copy <- option False $ do
    try $ do
      consume "!"
      return True
  try $ do
    skip
    consume "@"
  name <- parseName
  args <- option [] $ do
    try $ string "("
    args <- many $ do
      closeWith ")"
      bang <- option False $ do
        try $ do
          consume "!"
          return True
      arg <- parseName
      let strict = bang || head arg == '&'
      return (strict, arg)
    consume ")"
    return args
  skip
  consume "="
  core <- parseCore
  return (name, ((copy,args), core))

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

parseBook :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseBook = do
  skip
  many parseADT
  defs <- many parseDef
  skip
  eof
  return defs

doParseCore :: String -> IO Core
doParseCore code = case runParser parseCore (ParserState MS.empty MS.empty 0) "" code of
  Right core -> do
    return $ core
  Left  err  -> do
    showParseError "" code err
    return $ Ref "⊥" 0 []

doParseBook :: String -> IO Book 
doParseBook code = case runParser parseBookWithState (ParserState MS.empty MS.empty 0) "" code of
  Right (defs, st) -> do
    return $ createBook defs (parsedCtrToCid st) (parsedCtrToAri st)
  Left err -> do
    showParseError "" code err
    return $ Book MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty
  where
    parseBookWithState :: ParserM ([(String, ((Bool,[(Bool,String)]), Core))], ParserState)
    parseBookWithState = do
      defs <- parseBook
      st <- getState
      return (defs, st)

-- Helper Parsers
-- --------------

consume :: String -> ParserM String
consume str = skip >> string str

closeWith :: String -> ParserM ()
closeWith str = try $ do
  skip
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

genFreshLabel :: ParserM Word64
genFreshLabel = do
  st <- getState
  let lbl = freshLabel st
  putState st { freshLabel = lbl + 1 }
  return $ lbl + 0x800000

-- Adjusting
-- ---------

-- TODO: create a 'registerPrim' function that adds the following entries to the nameToId map:
-- "SUP" -> 0xFFFFFE
-- "DUP" -> 0xFFFFFF
-- its type must receive/return a map

createBook :: [(String, ((Bool,[(Bool,String)]), Core))] -> MS.Map String Word64 -> MS.Map String Int -> Book
createBook defs ctrToCid ctrToAri =
  let withPrims = \n2i -> MS.union n2i $ MS.fromList primitives
      nameToId' = withPrims $ MS.fromList $ zip (map fst defs) [0..]
      idToName' = MS.fromList $ map (\(k,v) -> (v,k)) $ MS.toList nameToId'
      idToFunc' = MS.fromList $ map (\(name, ((copy,args), core)) -> (mget nameToId' name, ((copy,args), lexify (setRefIds nameToId' core)))) defs
      idToLabs' = MS.fromList $ map (\(name, (_, core)) -> (mget nameToId' name, collectLabels core)) defs
  in Book idToFunc' idToName' idToLabs' nameToId' ctrToAri ctrToCid

-- Adds the function id to Ref constructors
setRefIds :: MS.Map String Word64 -> Core -> Core
setRefIds fids term = case term of
  Var nam       -> Var nam
  Let m x v b   -> Let m x (setRefIds fids v) (setRefIds fids b)
  Lam x bod     -> Lam x (setRefIds fids bod)
  App f x       -> App (setRefIds fids f) (setRefIds fids x)
  Sup l x y     -> Sup l (setRefIds fids x) (setRefIds fids y)
  Dup l x y v b -> Dup l x y (setRefIds fids v) (setRefIds fids b)
  Ctr cid fds   -> Ctr cid (map (setRefIds fids) fds)
  Mat x mov css -> Mat (setRefIds fids x) (map (\ (k,v) -> (k, setRefIds fids v)) mov) (map (\ (ctr,fds,cs) -> (ctr, fds, setRefIds fids cs)) css)
  Op2 op x y    -> Op2 op (setRefIds fids x) (setRefIds fids y)
  U32 n         -> U32 n
  Chr c         -> Chr c
  Era           -> Era
  Ref nam _ arg -> case MS.lookup nam fids of
    Just fid -> Ref nam fid (map (setRefIds fids) arg)
    Nothing  -> unsafePerformIO $ do
      putStrLn $ "error:unbound-ref @" ++ nam
      exitFailure

-- Collects all SUP/DUP labels used
collectLabels :: Core -> MS.Map Word64 ()
collectLabels term = case term of
  Var _               -> MS.empty
  U32 _               -> MS.empty
  Chr _               -> MS.empty
  Era                 -> MS.empty
  Ref _ _ args        -> MS.unions $ map collectLabels args
  Let _ _ val bod     -> MS.union (collectLabels val) (collectLabels bod)
  Lam _ bod           -> collectLabels bod
  App fun arg         -> MS.union (collectLabels fun) (collectLabels arg)
  Sup lab tm0 tm1     -> MS.insert lab () $ MS.union (collectLabels tm0) (collectLabels tm1)
  Dup lab _ _ val bod -> MS.insert lab () $ MS.union (collectLabels val) (collectLabels bod)
  Ctr _ fds           -> MS.unions $ map collectLabels fds
  Mat val mov css     -> MS.unions $ collectLabels val : map (collectLabels . snd) mov ++ map (\(_,_,bod) -> collectLabels bod) css
  Op2 _ x y           -> MS.union (collectLabels x) (collectLabels y)

-- Gives unique names to lexically scoped vars, unless they start with '$'.
-- Example: `λx λt (t λx(x) x)` will read as `λx0 λt1 (t1 λx2(x2) x0)`.
lexify :: Core -> Core
lexify term = evalState (go term MS.empty) 0 where
  fresh :: String -> State Int String
  fresh nam@('$':_) = return $ nam
  fresh nam         = do i <- get; put (i+1); return $ nam++"$"++show i

  extend :: String -> String -> MS.Map String String -> State Int (MS.Map String String)
  extend old@('$':_) new ctx = return $ ctx
  extend old         new ctx = return $ MS.insert old new ctx

  go :: Core -> MS.Map String String -> State Int Core
  go term ctx = case term of
    Var nam -> 
      return $ Var (MS.findWithDefault nam nam ctx)
    Ref nam fid arg -> do
      arg <- mapM (\x -> go x ctx) arg
      return $ Ref nam fid arg
    Let mod nam val bod -> do
      val  <- go val ctx
      nam' <- fresh nam
      ctx  <- extend nam nam' ctx
      bod  <- go bod ctx
      return $ Let mod nam' val bod
    Lam nam bod -> do
      nam' <- fresh nam
      ctx  <- extend nam nam' ctx
      bod  <- go bod ctx
      return $ Lam nam' bod
    App fun arg -> do
      fun <- go fun ctx
      arg <- go arg ctx
      return $ App fun arg
    Sup lab tm0 tm1 -> do
      tm0 <- go tm0 ctx
      tm1 <- go tm1 ctx
      return $ Sup lab tm0 tm1
    Dup lab dp0 dp1 val bod -> do
      val  <- go val ctx
      dp0' <- fresh dp0
      dp1' <- fresh dp1
      ctx  <- extend dp0 dp0' ctx
      ctx  <- extend dp1 dp1' ctx
      bod  <- go bod ctx
      return $ Dup lab dp0' dp1' val bod
    Ctr cid fds -> do
      fds <- mapM (\x -> go x ctx) fds
      return $ Ctr cid fds
    Mat val mov css -> do
      val' <- go val ctx
      mov' <- forM mov $ \ (k,v) -> do
        k' <- fresh k
        v  <- go v ctx
        return $ (k', v)
      css' <- forM css $ \ (ctr,fds,bod) -> do
        fds' <- mapM fresh fds
        ctx  <- foldM (\ ctx (fd,fd') -> extend fd fd' ctx) ctx (zip fds fds')
        ctx  <- foldM (\ ctx ((k,_),(k',_)) -> extend k k' ctx) ctx (zip mov mov')
        bod <- go bod ctx
        return (ctr, fds', bod)
      return $ Mat val' mov' css'
    Op2 op nm0 nm1 -> do
      nm0 <- go nm0 ctx
      nm1 <- go nm1 ctx
      return $ Op2 op nm0 nm1
    U32 n -> 
      return $ U32 n
    Chr c ->
      return $ Chr c
    Era -> 
      return Era

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

