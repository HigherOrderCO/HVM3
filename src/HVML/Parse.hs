-- //./Type.hs//

module HVML.Parse where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.State
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.IORef
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

data VarUsage = Bound | Used

data ParserState = ParserState
  { pCidToAri  :: MS.Map Word64 Word64
  , pCidToLen  :: MS.Map Word64 Word64
  , pCtrToCid  :: MS.Map String Word64
  , pCidToADT  :: MS.Map Word64 Word64
  , imported   :: MS.Map String ()
  , varUsages  :: MS.Map String VarUsage
  , freshLabel :: Word64
  }

type ParserM = ParsecT String ParserState IO

bindVars :: [String] -> ParserM m -> ParserM m
bindVars vars parse = do
  st <- getState
  let prev  = varUsages st
  -- scopeless vars, regular vars
  let (svars, rvars) = partition (\v -> head v == '$') vars

  -- bind all scopeless vars
  prev <- foldM
    (\usgs var -> do
      case MS.lookup var usgs  of
        Just Bound -> fail $ "Scopeless variable " ++ show var ++ " has already been bound"
        _          -> return $ MS.insert var Bound usgs
    )
    prev
    svars

  let tempBound = MS.fromList [(var, Bound) | var <- rvars]

  putState st {varUsages = MS.union tempBound prev}

  val <- parse

  st <- getState
  let curr  = varUsages st
  putState st {varUsages = MS.union (MS.difference curr tempBound) prev}

  return val

useVar :: String -> ParserM ()
useVar var = do
  st <- getState
  putState st {varUsages = MS.insert var Used (varUsages st)}

checkVar :: String -> ParserM ()
checkVar var = do
  st <- getState
  case (var, MS.lookup var $ varUsages st) of
    ('&' : _, Just _)     -> return () -- &-vars can be used multiple times
    ('$' : _, Nothing)    -> useVar var *> return () -- $-vars can be used before definition
    (_,       Just Bound) -> useVar var *> return ()
    (_,       Just Used)  -> fail $ "Variable " ++ show var ++ " used more than once"
    (_,       Nothing)    -> fail $ "Unbound var " ++ show var

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
      var <- parseName1
      bod <- bindVars [var] parseCore
      return $ Lam var bod

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
              checkVar ("&" ++ name)
              return $ Ref "SUP" _SUP_F_ [Var ("&" ++ name), tm0, tm1]
        Nothing -> do
          checkVar ("&" ++ name)
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
          bod <- bindVars [dp0, dp1] parseCore

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
          bod <- bindVars [fromMaybe "_" nam] parseCore
          case nam of
            Just nam -> return $ Let STRI nam val bod
            Nothing  -> return $ Let STRI "_" val bod

        '^' -> do
          consume "^"
          nam <- parseName1
          consume "="
          val <- parseCore
          bod <- bindVars [nam] parseCore
          return $ Let PARA nam val bod

        _ -> do
          nam <- parseName1
          consume "="
          val <- parseCore
          bod <- bindVars [nam] parseCore
          return $ Let LAZY nam val bod

    '#' -> parseCtr

    '~' -> parseMat

    '[' -> parseLst

    '\'' -> parseChr

    '"' -> parseStr '"'
    '`' -> parseStr '`'

    _ -> do
      name <- parseName1
      case reads (filter (/= '_') name) of
        [(num, "")] -> return $ U32 (fromIntegral (num :: Integer))
        _           -> checkVar name >>= \_ -> return $ Var name

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
  fds <- option [] $ do
    try $ consume "{"
    fds <- many $ do
      closeWith "}"
      parseCore
    consume "}"
    return fds
  return $ Ctr ('#':nam) fds

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
      Nothing -> checkVar key >> return (key, Var key)
  consume "{"
  css <- many $ bindVars (map fst mov) $ do
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
      bod <- bindVars fds parseCore
      return ('#':ctr, fds, bod)
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
          bod <- bindVars [nam] parseCore
          return ("_", [nam], bod)

  consume "}"
  css <- forM css $ \ (ctr, fds, bod) -> do
    st <- getState
    cid <- case ctr of
      ('#':_) -> case MS.lookup ctr (pCtrToCid st) of
        Nothing  -> fail $ "Constructor not defined: " ++ ctr
        Just cid -> return $ cid -- Constructor Case: sort by CID
      _ -> case reads ctr of
        [(num :: Word64, "")] -> return $ num -- Numeric Case: sort by value
        _                     -> return $ maxBound -- Default Case: always last
    return (cid, (ctr, fds, bod))
  css <- return $ map snd $ sortOn fst css
  -- Switch
  if (let (ctr, _, _) = head css in ctr == "0") then do
    return $ Mat val mov css
  -- Match with only 1 case: a default case (forbidden)
  else if length css == 1 && (let (ctr, _, _) = head css in ctr == "_") then do
    fail "Match with only a default case is not allowed."
  -- Match with a default case: turn into If-Let chain
  else if (let (ctr, _, _) = last css in ctr == "_") then do
    let defName = (let (_,[nm],_) = last css in nm)
    let ifLets  = intoIfLetChain (Var defName) mov (init css) defName (last css)
    return $ Let LAZY defName val ifLets
  -- Match with all cases covered
  else do
    st <- getState
    let adt = mget (pCtrToCid st) (let (ctr,_,_) = head css in ctr)
    let len = mget (pCidToLen st) adt
    if fromIntegral (length css) /= len then
      fail $ "Incorrect number of cases"
    else
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

parseStr :: Char -> ParserM Core
parseStr delim = do
  skip
  char delim
  str <- many (noneOf [delim])
  char delim
  return $ foldr (\c acc -> Ctr "#Cons" [Chr c, acc]) (Ctr "#Nil" []) str

parseLst :: ParserM Core
parseLst = do
  skip
  char '['
  elems <- many $ do
    closeWith "]"
    parseCore
  char ']'
  return $ foldr (\x acc -> Ctr "#Cons" [x, acc]) (Ctr "#Nil" []) elems

parseName :: ParserM String
parseName = skip >> many (alphaNum <|> char '_' <|> char '$' <|> char '&')

parseName1 :: ParserM String
parseName1 = skip >> many1 (alphaNum <|> char '_' <|> char '$' <|> char '&')

parseDef :: ParserM (String, ((Bool, [(Bool, String)]), Core))
parseDef = do
  copy <- option False $ do
    string "!"
    skip
    return True
  string "@"
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
  core <- bindVars (map snd args) parseCore
  -- trace ("parsed: " ++ showCore core) $ do
  return (name, ((copy,args), core))

parseADT :: ParserM ()
parseADT = do
  string "data"
  name <- parseName
  skip
  consume "{"
  constructors <- many parseADTCtr
  consume "}"
  st <- getState
  let baseCid  = fromIntegral $ MS.size (pCtrToCid st)
  let ctrToCid = zip (map fst constructors) [baseCid..]
  let cidToAri = map (\ (ctr,cid) -> (cid, fromIntegral . length . snd $ head $ filter ((== ctr) . fst) constructors)) ctrToCid
  let cidToLen = (baseCid, fromIntegral $ length constructors)
  let cidToADT = map (\ (_,cid) -> (cid, baseCid)) ctrToCid
  modifyState (\s -> s { pCtrToCid = MS.union (MS.fromList ctrToCid) (pCtrToCid s),
                         pCidToAri = MS.union (MS.fromList cidToAri) (pCidToAri s),
                         pCidToLen = MS.insert (fst cidToLen) (snd cidToLen) (pCidToLen s),
                         pCidToADT = MS.union (MS.fromList cidToADT) (pCidToADT s) })

parseADTCtr :: ParserM (String, [String])
parseADTCtr = do
  skip
  consume "#"
  name <- parseName
  st <- getState
  when (MS.member ('#':name) (pCtrToCid st)) $ do
    fail $ "Constructor '" ++ name ++ "' redefined"
  fields <- option [] $ do
    try $ consume "{"
    fds <- many $ do
      closeWith "}"
      parseName
    skip
    consume "}"
    return fds
  skip
  return ('#':name, fields)

parseBook :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseBook = do
  skip
  defs <- many $ do
    def <- choice [parseTopImp, parseTopADT, parseTopDef]
    skip
    return def
  try $ skip >> eof
  return $ concat defs

parseBookWithState :: ParserM ([(String, ((Bool, [(Bool,String)]), Core))], ParserState)
parseBookWithState = do
  defs <- parseBook
  state <- getState
  return (defs, state)

parseTopADT :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseTopADT = do
  parseADT
  return []

parseTopDef :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseTopDef = do
  def <- parseDef
  return [def]

-- FIXME: this is ugly code, improve it
parseTopImp :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseTopImp = do
  string "import"
  space
  path <- many1 (noneOf "\n\r")
  st <- getState
  case MS.lookup path (imported st) of
    Just _  -> return []
    Nothing -> do
      contents <- liftIO $ readFile path
      modifyState (\s -> s { imported = MS.insert path () (imported s) })
      st <- getState
      result <- liftIO $ runParserT parseBookWithState st path contents
      case result of
        Left err -> do
          liftIO $ showParseError path contents err
          fail $ "encountered the above error when importing file " ++ show path
        Right (importedDefs, importedState) -> do
          putState importedState
          skip
          return importedDefs

doParseBook :: String -> String -> IO Book
doParseBook filePath code = do
  result <- runParserT p (ParserState MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty 0) "" code
  case result of
    Right (defs, st) -> do
      return $ createBook defs (pCtrToCid st) (pCidToAri st) (pCidToLen st) (pCidToADT st)
    Left err -> do
      showParseError filePath code err
      return $ Book MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty
  where
    p = do
      defs <- parseBook
      st <- getState
      return (defs, st)

doParseCore :: String -> IO Core
doParseCore code = do
  result <- runParserT parseCore (ParserState MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty 0) "" code
  case result of
    Right core -> return core
    Left err -> do
      showParseError "" code err
      return $ Ref "⊥" 0 []

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
  when (lbl > 0x7FFF) $ do
    error "Label overflow: generated label would be too large"
  return $ lbl + 0x8000

-- Adjusting
-- ---------

createBook :: [(String, ((Bool,[(Bool,String)]), Core))] -> MS.Map String Word64 -> MS.Map Word64 Word64 -> MS.Map Word64 Word64 -> MS.Map Word64 Word64 -> Book
createBook defs ctrToCid cidToAri cidToLen cidToADT =
  let withPrims = \n2i -> MS.union n2i (MS.fromList primitives)
      nameList  = zip (map fst defs) (map fromIntegral [0..]) :: [(String, Word64)]
      namToFid' = withPrims (MS.fromList nameList)
      fidToNam' = MS.fromList (map (\(k,v) -> (v,k)) (MS.toList namToFid'))
      fidToFun' = MS.fromList (map (\(fn, ((cp,ars), cr)) -> (mget namToFid' fn, ((cp, ars), lexify (setRefIds namToFid' cr)))) defs)
      fidToLab' = MS.fromList (map (\(fn, ((_, _), cr)) -> (mget namToFid' fn, collectLabels cr)) defs)
      cidToCtr' = MS.fromList (map (\(ctr, cid) -> (cid, ctr)) (MS.toList ctrToCid))
  in Book
       { fidToFun = fidToFun'
       , fidToNam = fidToNam'
       , fidToLab = fidToLab'
       , namToFid = namToFid'
       , cidToAri = cidToAri
       , cidToCtr = cidToCtr'
       , ctrToCid = ctrToCid
       , cidToLen = cidToLen
       , cidToADT = cidToADT
       }

-- Adds the function id to Ref constructors
setRefIds :: MS.Map String Word64 -> Core -> Core
setRefIds fids term = case term of
  Var nam       -> Var nam
  Let m x v b   -> Let m x (setRefIds fids v) (setRefIds fids b)
  Lam x bod     -> Lam x (setRefIds fids bod)
  App f x       -> App (setRefIds fids f) (setRefIds fids x)
  Sup l x y     -> Sup l (setRefIds fids x) (setRefIds fids y)
  Dup l x y v b -> Dup l x y (setRefIds fids v) (setRefIds fids b)
  Ctr nam fds   -> Ctr nam (map (setRefIds fids) fds)
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

    Ctr nam fds -> do
      fds <- mapM (\x -> go x ctx) fds
      return $ Ctr nam fds

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
    let msgs = errorMessages err
        failMsg = [msg | Message msg <- msgs]
        expectedMsgs = [msg | Expect msg <- msgs, msg /= "space", msg /= "Comment"]
    in if not (null failMsg)
       then head failMsg
       else if null expectedMsgs
            then "syntax error"
            else intercalate " | " expectedMsgs

showParseError :: String -> String -> ParseError -> IO ()
showParseError filename input err = do
  let pos = errorPos err
  let lin = sourceLine pos
  let col = sourceColumn pos
  let errorMsg = extractExpectedTokens err
  putStr $ setSGRCode [SetConsoleIntensity BoldIntensity] ++ "\nPARSE_ERROR" ++ setSGRCode [Reset]
  putStr " ("
  putStr $ setSGRCode [SetUnderlining SingleUnderline] ++ filename ++ setSGRCode [Reset]
  putStrLn ")"
  if any isMessage (errorMessages err)
    then putStrLn $ "- " ++ errorMsg
    else do
      putStrLn $ "- expected: " ++ errorMsg
      putStrLn $ "- detected:"
  putStrLn $ highlightError (lin, col) (lin, col + 1) input
  where
    isMessage (Message _) = True
    isMessage _ = False

-- Debug
-- -----

parseLog :: String -> ParserM ()
parseLog msg = do
  pos <- getPosition
  remaining <- getInput
  let preview = "[[[" ++ Data.List.take 20 remaining ++ (if length remaining > 20 then "..." else "") ++ "]]]"
  trace ("[" ++ show pos ++ "] " ++ msg ++ "\nRemaining code: " ++ preview) $ return ()
