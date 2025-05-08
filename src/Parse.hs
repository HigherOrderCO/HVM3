{-./Type.hs-}

module Parse where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.State
import Data.Either (isLeft)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Maybe (fromMaybe)
import Data.Word
import Debug.Trace
import Highlight (highlightError)
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec hiding (State)
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.String
import Type
import qualified Data.Map.Strict as MS

-- Core Parsers
-- ------------

data ParserState = ParserState
  { pCidToAri  :: MS.Map Word16 Word16
  , pCidToLen  :: MS.Map Word16 Word16
  , pCtrToCid  :: MS.Map String Word16
  , pCidToADT  :: MS.Map Word16 Word16
  , imported   :: MS.Map String ()
  , bindDups   :: MS.Map String (Int, (Core -> Core))
  , globalVars :: MS.Map String ()
  , pFreshLab  :: Lab
  }

type ParserM = ParsecT String ParserState IO

-- Core Term
parseCore :: ParserM Core
parseCore = do
  skip
  head <- lookAhead anyChar
  case head of
    '*'  -> parseEra
    'λ'  -> parseLam
    '('  -> parseExp
    '@'  -> parseRef
    '&'  -> parseSup
    '!'  -> parseLet
    '#'  -> parseCtr
    '~'  -> parseMat
    '↑'  -> parseInc
    '↓'  -> parseDec
    '['  -> parseLst
    '\'' -> parseChr
    '"'  -> parseStr '"'
    '`'  -> parseStr '`'
    _    -> parseLit

-- Era: `*`
parseEra :: ParserM Core
parseEra = do
  consume "*"
  return Era

-- Lam: `λx.F` or `λ&x.F` for non-linear variables
parseLam :: ParserM Core
parseLam = do
  consume "λ"
  var <- parseName1
  swallow "."
  bod <- bindVars [var] parseCore
  return $ Lam (stripName var) bod

-- FshSup: `& {a,b}` -- uses a fresh label
-- StaSup: `&0{a,b}` -- uses a static label
-- DynSup: `&L{a,b}` -- uses a dynamic label
-- DynLab: `&L`      -- a dynamic label variable
parseSup :: ParserM Core
parseSup = do
  consume "&"
  name <- parseName
  next <- optionMaybe $ try $ lookAhead anyChar
  case next of
    Just '{' -> do
      consume "{"
      tm0 <- parseCore
      swallow ","
      tm1 <- parseCore
      consume "}"
      if null name then do
        num <- genFreshLabel
        return $ Sup num tm0 tm1
      else case reads name of
        [(num :: Lab, "")] -> do
          return $ Sup num tm0 tm1
        otherwise -> do
          var <- makeVar name
          return $ Ref "SUP" (fromIntegral _SUP_F_) [var, tm0, tm1]
    _ -> makeVar ("&" ++ name)

-- Exp: `(<op> A B)`
parseExp :: ParserM Core
parseExp = do
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
      skip
      char ')'
      return $ foldl (\f a -> App f a) fun args

-- Oper: `(+ a b)`
parseOper :: Oper -> ParserM Core
parseOper op = do
  consume "("
  consume (show op)
  nm0 <- parseCore
  nm1 <- parseCore
  consume ")"
  return $ Op2 op nm0 nm1

-- Ref: `@Fun(x0 x1 ...)`
parseRef :: ParserM Core
parseRef = do
  consume "@"
  name <- parseName1
  args <- option [] $ do
    try $ string "("
    args <- many $ do
      closeWith ")"
      swallow ","
      parseCore
    consume ")"
    return args
  return $ Ref name 0 args

-- Ctr: `#Ctr{x0 x1 ...}`
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

-- Mat: `~ x !m0=v0 !m1=v1 ... { #Ctr{x0 x1 ...}:... ... }`
parseMat :: ParserM Core
parseMat = do
  consume "~"
  val <- parseCore
  mov <- many parseMove
  consume "{"
  cs0 <- parseCase False (map fst mov)
  css <- many $ parseCase (let (n,_,_)=cs0 in n=="0") (map fst mov)
  consume "}"
  css <- sortCases (cs0:css)
  buildMatchExpr val mov css

-- Mov: `!m0 = v0` (used inside Mat)
parseMove :: ParserM (String, Core)
parseMove = do
  try $ skip >> consume "!"
  name <- parseName1
  expr <- optionMaybe $ try $ consume "=" >> parseCore
  case expr of
    Just e  -> return (name, e)
    Nothing -> do
      var <- makeVar (stripName name)
      return (name, var)  -- !x is shorthand for !x=x

-- Case: CtrCase | NumCase | DefCase
parseCase :: Bool -> [String] -> ParserM (String, [String], Core)
parseCase isNumMat mov = do
  closeWith "}" >> skip
  c <- lookAhead anyChar
  if c == '#'
    then parseCtrCase mov -- Constructor case
    else if c >= '0' && c <= '9'
      then parseNumCase mov -- Numeric case
      else (parseDefCase isNumMat mov) -- Default case

-- CtrCase: `#Ctr{x0 x1 ...}: f`
parseCtrCase :: [String] -> ParserM (String, [String], Core)
parseCtrCase mov = do
  consume "#"
  name <- parseName1
  vars <- option [] $ do
    consume "{"
    vars <- many $ do
      closeWith "}"
      parseName1
    consume "}"
    return vars
  consume ":"
  body <- bindVars (mov ++ vars) parseCore
  swallow ";"
  return ('#':name, map stripName vars, body)

-- NumCase: LitCase | PreCase
parseNumCase :: [String] -> ParserM (String, [String], Core)
parseNumCase mov = try (parseLitCase mov) <|> try (parsePreCase mov)

-- LitCase: `123: f`
parseLitCase :: [String] -> ParserM (String, [String], Core)
parseLitCase mov = do
  digits <- many1 digit
  consume ":"
  body <- bindVars mov parseCore
  swallow ";"
  return (digits, [], body)

-- PreCase: `123+p: f`
parsePreCase :: [String] -> ParserM (String, [String], Core)
parsePreCase mov = do
  pred <- many1 digit
  consume "+"
  name <- parseName1
  consume ":"
  body <- bindVars (mov ++ [name]) parseCore
  swallow ";"
  return ("_", [stripName name], body)

-- DefCase: `x: f`
parseDefCase :: Bool -> [String] -> ParserM (String, [String], Core)
parseDefCase isNumMat mov = do
  name <- parseName1
  consume ":"
  body <- bindVars (mov ++ [name]) parseCore
  swallow ";"
  if isNumMat && name /= "_" then do
    fail $ concat
      [ "To avoid ambiguity, the switch syntax changed.\n"
      , "- Old Syntax: ~ n { 0:zero_case x:pred_case }\n"
      , "- New Syntax: ~ n { 0:zero_case 1+x:pred_case }\n"
      , "- Please, update your code."
      ]
  else do
    return ("_", [stripName name], body)

-- Inc: `↑x`
parseInc :: ParserM Core
parseInc = do
  consume "↑"
  term <- parseCore
  return $ Inc term

-- Dec: `↓x`
parseDec :: ParserM Core
parseDec = do
  consume "↓"
  term <- parseCore
  return $ Dec term

-- Let: Dup | StriLet | LazyLet
parseLet :: ParserM Core
parseLet = do
  consume "!"
  skip
  next <- lookAhead anyChar
  case next of
    '&' -> try parseDup <|> try parseLazyLet
    '!' -> parseStriLet
    _   -> parseLazyLet

-- Fresh Dup   : `! & {a b}=v f`
-- Static Dup  : `! &0{a b}=v f`
-- Dynamic Dup : `! &L{a b}=v f`
parseDup :: ParserM Core
parseDup = do
  consume "&"
  nam <- parseName
  consume "{"
  dp0 <- parseName1
  dp1 <- parseName1
  consume "}"
  consume "="
  val <- parseCore
  swallow ";"
  bod <- bindVars [dp0, dp1] parseCore
  if null nam then do
    num <- genFreshLabel
    return $ Dup num (stripName dp0) (stripName dp1) val bod
  else case reads nam of
    [(num :: Lab, "")] -> do
      return $ Dup num (stripName dp0) (stripName dp1) val bod
    otherwise -> do
      var <- makeVar nam
      return $ Ref "DUP" (fromIntegral _DUP_F_) [var, val, Lam (stripName dp0) (Lam (stripName dp1) bod)]

-- StriLet: `! !x=v f`
parseStriLet :: ParserM Core
parseStriLet = do
  consume "!"
  nam <- option "_" $ try $ do
    nam <- parseName1
    consume "="
    return nam
  val <- parseCore
  swallow ";"
  bod <- bindVars [nam] parseCore
  return $ Let STRI (stripName nam) val bod

-- LazyLet: `! x=v f`
parseLazyLet :: ParserM Core
parseLazyLet = do
  nam <- parseName1
  consume "="
  val <- parseCore
  swallow ";"
  bod <- bindVars [nam] parseCore
  return $ Let LAZY (stripName nam) val bod

-- Lit: Var | U32
parseLit :: ParserM Core
parseLit = do
  name <- parseName1
  case reads (filter (/= '_') name) of
    [(num, "")] -> return $ U32 (fromIntegral (num :: Integer))
    _           -> makeVar name

-- Chr: 'x'
parseChr :: ParserM Core
parseChr = do
  skip
  char '\''
  c <- escaped
  char '\''
  return $ Chr c

-- -- Str: "abc"
parseStr :: Char -> ParserM Core
parseStr delim = do
  skip
  char delim
  str <- many escaped
  char delim
  return $ foldr (\c acc -> Ctr "#Cons" [Chr c, acc]) (Ctr "#Nil" []) str

-- Lst: `[x0 x1 ...]`
parseLst :: ParserM Core
parseLst = do
  skip
  char '['
  elems <- many $ do
    closeWith "]"
    swallow ","
    parseCore
  skip
  char ']'
  return $ foldr (\x acc -> Ctr "#Cons" [x, acc]) (Ctr "#Nil" []) elems

-- Def: `@foo(x0 x1...) = f`
parseDef :: ParserM (String, ((Bool, [(Bool, String)]), Core))
parseDef = do
  -- Reset global binds
  modifyState $ \st -> st { globalVars = MS.empty, bindDups = MS.empty }
  copy <- option False $ do
    string "!"
    skip
    return True
  string "@"
  name <- parseName1
  args <- option [] $ do
    try $ string "("
    args <- many $ do
      closeWith ")"
      swallow ","
      strict <- option False $ do
        try $ do
          consume "!"
          return True
      arg <- parseName1
      return (strict, arg)
    consume ")"
    return args
  skip
  consume "="
  core <- bindVars (map snd args) parseCore
  let args' = map (\(strict, arg) -> (strict, stripName arg)) args
  return (name, ((copy,args'), core))

-- ADT: `data Foo { #Ctr{x0 x1 ...} ... }`
parseADT :: ParserM ()
parseADT = do
  string "data"
  name <- parseName1
  skip
  consume "{"
  constructors <- many parseADTCtr
  consume "}"
  registerADT name constructors

-- ADT-Ctr: `#Ctr{x0 x1 ...}`
parseADTCtr :: ParserM (String, [String])
parseADTCtr = do
  skip
  consume "#"
  name <- parseName1
  st <- getState
  when (MS.member ('#':name) (pCtrToCid st)) $ do
    fail $ "Constructor '" ++ name ++ "' redefined"
  fields <- option [] $ do
    try $ consume "{"
    fds <- many $ do
      closeWith "}"
      parseName1
    skip
    consume "}"
    return fds
  skip
  return ('#':name, fields)

-- Book: [ADT]
parseBook :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseBook = do
  skip
  defs <- many $ do
    def <- choice [parseTopImp, parseTopADT, parseTopDef]
    skip
    return def
  try $ skip >> eof
  return $ concat defs

-- TopADT: ADT
parseTopADT :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseTopADT = do
  parseADT
  return []

-- TopDef: Def
parseTopDef :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseTopDef = do
  def <- parseDef
  return [def]

-- TopImp: 'import Foo/bar.hvm'
parseTopImp :: ParserM [(String, ((Bool, [(Bool,String)]), Core))]
parseTopImp = do
  string "import"
  space
  path <- many1 (noneOf "\n\r")
  st <- getState
  if MS.member path (imported st)
    then return [] -- skip if already imported
    else importFile path
  where
  importFile :: String -> ParserM [(String, ((Bool, [(Bool,String)]), Core))]
  importFile path = do
    modifyState (\s -> s { imported = MS.insert path () (imported s) })
    contents <- liftIO $ readFile path
    st       <- getState
    result   <- liftIO $ runParserT parseBookWithState st path contents
    case result of
      Left err -> handleParseError path contents err
      Right (importedDefs, importedState) -> do
        putState importedState
        skip
        return importedDefs
  parseBookWithState :: ParserM ([(String, ((Bool, [(Bool,String)]), Core))], ParserState)
  parseBookWithState = do
    defs <- parseBook
    state <- getState
    return (defs, state)

-- Utils
-- -----

parseName :: ParserM String
parseName = skip >> many (alphaNum <|> char '_' <|> char '$' <|> char '&')

parseName1 :: ParserM String
parseName1 = skip >> many1 (alphaNum <|> char '_' <|> char '$' <|> char '&')

consume :: String -> ParserM String
consume str = skip >> string str

swallow :: String -> ParserM ()
swallow str = do
  skip
  _ <- optionMaybe $ try (string str)
  return ()

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
    (char '\n' >> return ()) <|> eof
    return ()) <?> "Comment"

escaped :: ParserM Char
escaped
  =   parseEscapeSequence
  <|> parseUnicodeEscape
  <|> parseRegularChar
  where
  parseEscapeSequence = try $ do
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
  parseUnicodeEscape = try $ do
    string "\\u"
    code <- count 4 hexDigit
    return $ toEnum (read ("0x" ++ code) :: Int)
  parseRegularChar = noneOf "\"\\"

-- External API
-- ------------

-- Parse Book and Core
doParseBook filePath code = do
  result <- runParserT p (ParserState MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty 0) "" code
  case result of
    Right (defs, st) -> do
      return $ createBook defs (pCtrToCid st) (pCidToAri st) (pCidToLen st) (pCidToADT st) (pFreshLab st)
    Left err -> do
      showParseError filePath code err
      return $ Book MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty 0
  where
    p = do
      defs <- parseBook
      st <- getState
      return (defs, st)

doParseCore :: String -> IO Core
doParseCore code = do
  result <- runParserT parseCore (ParserState MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty MS.empty 0) "" code
  case result of
    Right core -> return core
    Left err -> do
      showParseError "" code err
      return $ Ref "⊥" 0 []

doParseArgument :: String -> Book -> IO (Book, Core)
doParseArgument code book = do
  result <- runParserT p (ParserState 
    { pCidToAri = cidToAri book
    , pCidToLen = cidToLen book
    , pCtrToCid = ctrToCid book
    , pCidToADT = cidToADT book
    , imported  = MS.empty
    , bindDups  = MS.empty
    , globalVars = MS.empty
    , pFreshLab = freshLab book
    }) "" code
  case result of
    Right (core, st) -> do
      let book' = book { freshLab = pFreshLab st }
      return (book', core)
    Left err -> do
      showParseError "" code err
      return (book, Ref "⊥" 0 [])
  where
      p = do
        core <- parseCore
        st <- getState
        let core' = lexify (setRefIds (namToFid book) core)
        return (core', st)

-- Errors
-- ------

handleParseError :: String -> String -> ParseError -> ParserM a
handleParseError path contents err = do
  liftIO $ showParseError path contents err
  fail $ "Error importing file " ++ show path ++ ": parse failed"

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

parseLog :: String -> ParserM ()
parseLog msg = do
  pos <- getPosition
  remaining <- getInput
  let preview = "[[[" ++ Data.List.take 20 remaining ++ (if length remaining > 20 then "..." else "") ++ "]]]"
  trace ("[" ++ show pos ++ "] " ++ msg ++ "\nRemaining code: " ++ preview) $ return ()

-- Book
-- ----

-- Register the parsed ADT in the parser state
registerADT :: String -> [(String, [String])] -> ParserM ()
registerADT name constructors = do
  st <- getState
  let baseCid  = fromIntegral $ MS.size (pCtrToCid st)
  let ctrToCid = zip (map fst constructors) [baseCid..]
  let cidToAri = map (\ (ctr,cid) -> (cid, fromIntegral . length . snd $ head $ filter ((== ctr) . fst) constructors)) ctrToCid
  let cidToLen = (baseCid, fromIntegral $ length constructors)
  let cidToADT = map (\ (_,cid) -> (cid, baseCid)) ctrToCid
  modifyState (\s -> s {
    pCtrToCid = MS.union (MS.fromList ctrToCid) (pCtrToCid s),
    pCidToAri = MS.union (MS.fromList cidToAri) (pCidToAri s),
    pCidToLen = MS.insert (fst cidToLen) (snd cidToLen) (pCidToLen s),
    pCidToADT = MS.union (MS.fromList cidToADT) (pCidToADT s) })

-- Book creation and setup functions
createBook :: [(String, ((Bool,[(Bool,String)]), Core))] -> MS.Map String Word16 -> MS.Map Word16 Word16 -> MS.Map Word16 Word16 -> MS.Map Word16 Word16 -> Lab -> Book
createBook defs ctrToCid cidToAri cidToLen cidToADT freshLab =
  let withPrims = \n2i -> MS.union n2i (MS.fromList primitives)
      nameList  = zip (map fst defs) [0..] :: [(String, Word16)]
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
       , freshLab = freshLab
       }

-- Binding
-- -------

-- Strip the & prefix from a non-linear variable name
-- e.g., "&x" -> "x", "x" -> "x"
stripName :: String -> String
stripName var = if not (null var) && head var == '&' then tail var else var

bindVars :: [String] -> ParserM Core -> ParserM Core
bindVars vars parse = do
  st <- getState
  let prev = bindDups st
  -- Split into scopeless vars (starting with $) and regular vars
  let (svars, rvars) = partition (\v -> head v == '$') vars

  -- Bind all scopeless vars
  boundScopeless <- foldM bindScopelessVar prev svars

  -- Create temporary bindings for regular vars
  let tempBound = MS.fromList [(stripName var, (0, (\x -> x))) | var <- rvars]

  -- Update parser state with all bindings
  putState st {bindDups = MS.union tempBound boundScopeless}

  -- Run the parser with the new bindings
  body <- parse

  st' <- getState
  let curr = bindDups st'

  -- Add the duplications to the body and check linear variables - error if used more than once
  result <- foldM (applyDups curr) body (reverse rvars)

  -- Restore the original state for regular vars
  putState st' {bindDups = MS.union (MS.difference curr tempBound) prev}

  return result

  where
    -- Helper to bind a scopeless variable
    bindScopelessVar usages var = do
      st <- getState
      case MS.lookup var (globalVars st) of
        Just _ -> fail $ "Global variable " ++ show var ++ " already bound"
        Nothing -> do
          putState st {globalVars = MS.insert var () (globalVars st)}
          return usages

    -- Helper to apply the duplications to the body
    applyDups usages body var = do
      let (uses, dups) = mget usages (stripName var)
      if (head var /= '&') && (uses > 1) then
        fail $ "Linear variable " ++ show var ++ " used " ++ show uses ++ " times"
      else
        return (dups body)

makeVar :: String -> ParserM Core
makeVar name = do
  st <- getState
  case (head name, MS.lookup name (bindDups st)) of
    ('$', Nothing) -> do -- $-vars can be used before definition
      putState st {bindDups = MS.insert name (1, (\x -> x)) (bindDups st)}
      return $ Var name
    (_, Nothing) -> do
      fail $ "Unbound variable " ++ show name
    (_, Just (uses, dups)) -> do
      -- Automatically add duplications if the variable is used more than once
      label <- genFreshLabel
      let dupName = if uses == 0
                    then name
                    else name ++ "_dup" ++ show uses
      let dups' = if uses == 0
                  then dups
                  else \x -> Dup label name dupName (Var name) (dups x)
      modifyState (\s -> s {bindDups = MS.insert name (uses + 1, dups') (bindDups s)})
      return $ Var dupName

-- Adjusters and Utils
-- -------------------

genFreshLabel :: ParserM Lab
genFreshLabel = do
  st <- getState
  let lbl = pFreshLab st
  putState st { pFreshLab = lbl + 1 }
  when (lbl > 0x7FFFFF) $ do
    error "Label overflow: generated label would be too large"
  return $ lbl + 0x800000

-- Adds the function id to Ref constructors
setRefIds :: MS.Map String Word16 -> Core -> Core
setRefIds fids term = case term of
  Var nam         -> Var nam
  Let m x v b     -> Let m x (setRefIds fids v) (setRefIds fids b)
  Lam x bod       -> Lam x (setRefIds fids bod)
  App f x         -> App (setRefIds fids f) (setRefIds fids x)
  Sup l x y       -> Sup l (setRefIds fids x) (setRefIds fids y)
  Dup l x y v b   -> Dup l x y (setRefIds fids v) (setRefIds fids b)
  Ctr nam fds     -> Ctr nam (map (setRefIds fids) fds)
  Mat k x mov css -> Mat k (setRefIds fids x) (map (\ (k,v) -> (k, setRefIds fids v)) mov) (map (\ (ctr,fds,cs) -> (ctr, fds, setRefIds fids cs)) css)
  Op2 op x y      -> Op2 op (setRefIds fids x) (setRefIds fids y)
  U32 n           -> U32 n
  Chr c           -> Chr c
  Era             -> Era
  Inc x           -> Inc (setRefIds fids x)
  Dec x           -> Dec (setRefIds fids x)
  Ref nam _ arg   -> case MS.lookup nam fids of
    Just fid -> Ref nam fid (map (setRefIds fids) arg)
    Nothing  -> unsafePerformIO $ do
      putStrLn $ "error:unbound-ref @" ++ nam
      exitFailure

-- Collects all labels used
collectLabels :: Core -> MS.Map Lab ()
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
  Mat kin val mov css -> MS.unions $ collectLabels val : map (collectLabels . snd) mov ++ map (\(_,_,bod) -> collectLabels bod) css
  Op2 _ x y           -> MS.union (collectLabels x) (collectLabels y)
  Inc x               -> collectLabels x
  Dec x               -> collectLabels x

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
    Mat kin val mov css -> do
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
      return $ Mat kin val' mov' css'
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
    Inc x -> do
      x <- go x ctx
      return $ Inc x
    Dec x -> do
      x <- go x ctx
      return $ Dec x

-- Sorts cases by constructor ID or numeric value
sortCases :: [(String, [String], Core)] -> ParserM [(String, [String], Core)]
sortCases cases = do
  st <- getState
  sorted <- forM cases $ \c@(name, fds, _) -> do
    key <- case name of
      ('#':_) -> case MS.lookup name (pCtrToCid st) of
        Nothing -> fail $ "Constructor not defined: " ++ name
        Just id -> return id  -- Sort constructor cases by ID
      _ -> return $ case reads name of
        [(num :: Word16, "")] -> num      -- Sort numeric cases by value
        _                     -> maxBound -- Default case goes last
    return (key, c)
  return $ map snd $ sortOn fst sorted

-- Build match expression based on case types
buildMatchExpr :: Core -> [(String, Core)] -> [(String, [String], Core)] -> ParserM Core
buildMatchExpr val mov cases
  | null cases = 
      fail "Match needs at least one case"
  | isSwitch (head cases) = 
      return $ Mat SWI val (stripMov mov  ) cases  -- Switch case
  | onlyDefault cases =
      fail "Match with only a default case is not allowed"  -- Invalid case
  | hasDefault (last cases) = do  -- Has default: use If-Let chain
      st  <- getState
      adt <- return $ mget (pCtrToCid st) (getName $ head cases)
      var <- return $ getVar (last cases)
      ifl <- intoIfLetChain (Var var) (stripMov mov) (init cases) var (last cases)
      return $ Let LAZY var val ifl
  | otherwise = do  -- All ADT cases covered
      st <- getState
      let adt = mget (pCtrToCid st) (getName $ head cases)
      let len = mget (pCidToLen st) adt
      if fromIntegral (length cases) /= len
        then fail "Incorrect number of cases"
        else return $ Mat (MAT adt) val (stripMov mov) cases
  where
    isSwitch (name, _, _) = name == "0"
    hasDefault (name, _, _) = name == "_"
    onlyDefault cases = length cases == 1 && hasDefault (head cases)
    getName (name, _, _) = name
    getVar (_, [v], _) = v
    stripMov mov = map (\(nm,cr) -> (stripName nm, cr)) mov

intoIfLetChain :: Core -> [(String, Core)] -> [(String, [String], Core)] -> String -> (String, [String], Core) -> ParserM Core
intoIfLetChain _ _ [] defName (_,_,defBody) = return defBody
intoIfLetChain val mov ((ctr,fds,bod):css) defName defCase = do
  st  <- getState
  kin <- return $ IFL (mget (pCtrToCid st) ctr)
  rec <- intoIfLetChain val mov css defName defCase
  css <- return $ [(ctr, fds, bod), ("_", [defName], rec)]
  return $ Mat kin val mov css
