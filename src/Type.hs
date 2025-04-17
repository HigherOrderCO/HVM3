module Type where

import Data.Word
import Foreign.Ptr

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Char (chr, ord)
import Data.Char (intToDigit)
import Data.IORef
import Data.List
import Data.Word
import Numeric (showIntAtBase)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as MS hiding (map)

-- Core Types
-- ----------

type Tag  = Word8
type Lab  = Word32
type Loc  = Word32
type Term = Word64

type Name = String
type Move = (Name, Core) -- !x = term
type Case = (Name, [Name], Core) -- #Ctr{x0 x1...}: fn

data LetT = LAZY | STRI deriving (Eq, Enum)
data MatT = SWI | MAT Word16 | IFL Word16 deriving (Show, Eq)

data Core
  = Var Name                    -- x
  | Ref Name Word16 [Core]      -- @fn
  | Era                         -- *
  | Lam Name Core               -- λx(F)
  | App Core Core               -- (f x)
  | Sup Lab Core Core           -- &L{a b}
  | Dup Lab Name Name Core Core -- ! &L{a b} = v body
  | Ctr Name [Core]             -- #Ctr{a b ...}
  | U32 Word32                  -- 123
  | Chr Char                    -- 'a'
  | Op2 Oper Core Core          -- (+ a b)
  | Let LetT Name Core Core     -- ! x = v body
  | Mat MatT Core [Move] [Case] -- ~ v !moves { cases }
  deriving (Eq)

data Oper
  = OP_ADD | OP_SUB | OP_MUL | OP_DIV
  | OP_MOD | OP_EQ  | OP_NE  | OP_LT
  | OP_GT  | OP_LTE | OP_GTE | OP_AND
  | OP_OR  | OP_XOR | OP_LSH | OP_RSH
  deriving (Eq, Enum)

-- A top-level function, including:
-- - copy: true when ref-copy mode is enabled
-- - args: a list of (isArgStrict, argName) pairs
-- - core: the function's body
-- Note: ref-copy improves C speed, but increases interaction count
type Func = ((Bool, [(Bool,String)]), Core)

-- Set of labels in a function's body
type HasLab = (MS.Map Lab ())

data Book = Book
  { fidToFun :: MS.Map Word16 Func   -- func id to Func object
  , fidToLab :: MS.Map Word16 HasLab -- func id to dup labels used
  , fidToNam :: MS.Map Word16 Name   -- func id to name
  , namToFid :: MS.Map Name   Word16 -- func name to id
  , cidToAri :: MS.Map Word16 Word16 -- ctor id to field count (arity)
  , cidToLen :: MS.Map Word16 Word16 -- ctor id to cases length (ADT ctors)
  , cidToCtr :: MS.Map Word16 Name   -- ctor id to name
  , ctrToCid :: MS.Map Name   Word16 -- ctor name to id
  , cidToADT :: MS.Map Word16 Word16 -- ctor id to ADT id (first ADT cid)
  } deriving (Show, Eq)

-- Runtime Types
-- -------------

type HVM = IO
type ReduceAt = Book -> Loc -> HVM Term

-- Constants
-- ---------

-- Tags
_DP0_ = 0x00 :: Tag
_DP1_ = 0x01 :: Tag
_VAR_ = 0x02 :: Tag
_FWD_ = 0x03 :: Tag
_REF_ = 0x04 :: Tag
_LET_ = 0x05 :: Tag
_APP_ = 0x06 :: Tag
_MAT_ = 0x08 :: Tag
_IFL_ = 0x09 :: Tag
_SWI_ = 0x0A :: Tag
_OPX_ = 0x0B :: Tag
_OPY_ = 0x0C :: Tag
_ERA_ = 0x0D :: Tag
_LAM_ = 0x0E :: Tag
_SUP_ = 0x0F :: Tag
_CTR_ = 0x10 :: Tag
_W32_ = 0x11 :: Tag
_CHR_ = 0x12 :: Tag

-- Let Types
modeT :: Lab -> LetT
modeT 0x00 = LAZY
modeT 0x01 = STRI
modeT mode = error $ "unknown mode: " ++ show mode

-- Primitive Functions
_DUP_F_ = 0xFFFF :: Lab
_SUP_F_ = 0xFFFE :: Lab
_LOG_F_ = 0xFFFD :: Lab

primitives :: [(String, Word16)]
primitives =
  [ ("SUP", fromIntegral _SUP_F_)
  , ("DUP", fromIntegral _DUP_F_)
  , ("LOG", fromIntegral _LOG_F_)
  ]

-- Utils
-- -----

-- Getter function for maps
-- TODO: add the type annotation for mget
mget :: (Ord k, Show k) => MS.Map k a -> k -> a
mget map key =
  case MS.lookup key map of
    Just val -> val
    Nothing  -> error $ "key not found: " ++ show key

funArity :: Book -> Word16 -> Word16
funArity book fid
  | fid == fromIntegral _SUP_F_ = 3
  | fid == fromIntegral _DUP_F_ = 3
  | fid == fromIntegral _LOG_F_ = 1
  | otherwise = case MS.lookup fid (fidToFun book) of
      Just ((_, args), _) -> fromIntegral (length args)
      Nothing -> error $ "Function ID not found: " ++ show fid

-- Stringification
-- ---------------

padLeft :: String -> Int -> Char -> String
padLeft str n c = replicate (n - length str) c ++ str

showHex :: Word64 -> String
showHex x = showIntAtBase 16 intToDigit (fromIntegral x) ""

showName :: Int -> String
showName n = go (n + 1) "" where
  go n ac | n == 0    = ac
          | otherwise = go q (chr (ord 'a' + r) : ac)
          where (q,r) = quotRem (n - 1) 26

showTag :: Tag -> String
showTag tag
  | tag == _DP0_ = "DP0"
  | tag == _DP1_ = "DP1"
  | tag == _VAR_ = "VAR"
  | tag == _FWD_ = "FWD"
  | tag == _REF_ = "REF"
  | tag == _LET_ = "LET"
  | tag == _APP_ = "APP"
  | tag == _MAT_ = "MAT"
  | tag == _IFL_ = "IFL"
  | tag == _SWI_ = "SWI"
  | tag == _OPX_ = "OPX"
  | tag == _OPY_ = "OPY"
  | tag == _ERA_ = "ERA"
  | tag == _LAM_ = "LAM"
  | tag == _SUP_ = "SUP"
  | tag == _CTR_ = "CTR"
  | tag == _W32_ = "W32"
  | tag == _CHR_ = "CHR"
  | otherwise    = error $ "unknown tag: " ++ show tag

showLab :: Lab -> String
showLab lab = padLeft (showHex (fromIntegral lab)) 6 '0'

showLoc :: Loc -> String
showLoc loc = padLeft (showHex (fromIntegral loc)) 8 '0'

instance Show Oper where
  show OP_ADD = "+"
  show OP_SUB = "-"
  show OP_MUL = "*"
  show OP_DIV = "/"
  show OP_MOD = "%"
  show OP_EQ  = "=="
  show OP_NE  = "!="
  show OP_LT  = "<"
  show OP_GT  = ">"
  show OP_LTE = "<="
  show OP_GTE = ">="
  show OP_AND = "&"
  show OP_OR  = "|"
  show OP_XOR = "^"
  show OP_LSH = "<<"
  show OP_RSH = ">>"

instance Show LetT where
  show LAZY = ""
  show STRI = "."

showCore :: Core -> String
showCore core = maybe (format core) id (sugar core) where

  sugar :: Core -> Maybe String
  sugar core = nil core <|> str core <|> lst core where
    nil :: Core -> Maybe String
    nil (Ctr "#Nil" []) = Just "[]"
    nil _               = Nothing
    str :: Core -> Maybe String
    str (Ctr "#Nil" []) = Just "\"\""
    str (Ctr "#Cons" [Chr h, t]) = do
      rest <- str t
      return $ "\"" ++ h : tail rest
    str _ = Nothing
    lst :: Core -> Maybe String
    lst (Ctr "#Nil" [])       = Just "[]"
    lst (Ctr "#Cons" [x, xs]) = do
      rest <- lst xs
      return $ "[" ++ showCore x ++ if rest == "[]" then "]" else " " ++ tail rest
    lst _ = Nothing

  format :: Core -> String
  format (Var k) =
    k
  format Era =
    "*"
  format (Lam x f) =
    let f' = showCore f in
    concat ["λ", x, " ", f']
  format (App f x) =
    let f' = showCore f in
    let x' = showCore x in
    concat ["(", f', " ", x', ")"]
  format (Sup l a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["&", show l, "{", a', " ", b', "}"]
  format (Dup l x y v f) =
    let v' = showCore v in
    let f' = showCore f in
    concat ["! &", show l, "{", x, " ", y, "} = ", v', "\n", f']
  format (Ref k i xs) =
    let xs' = intercalate " " (map showCore xs) in
    concat ["@", k, "(", xs', ")"]
  format (Ctr k xs) =
    let xs' = unwords (map showCore xs) in
    concat [k, "{", xs', "}"]
  format (Mat k v m ks) =
    let v'  = showCore v in
    let m'  = concatMap (\(k,v) -> concat [" !", k, "=", showCore v]) m in
    let ks' = unwords [concat [c, ":", showCore b] | (c, _, b) <- ks] in
    concat ["(~", v', m', " {", ks', "})"]
  format (U32 v) =
    show v
  format (Chr v) =
    concat ["'", [v], "'"]
  format (Op2 o a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["(", show o, " ", a', " ", b', ")"]
  format (Let m k v f)
    | k == "" =
      let v' = showCore v in
      let f' = showCore f in
      concat [v', "\n", f']
    | otherwise =
      let v' = showCore v in
      let f' = showCore f in
      concat ["! ", show m, k, " = ", v', "\n", f']

rename :: Core -> Core
rename core = unsafePerformIO $ do
  names <- newIORef MS.empty
  renamer names core

renamer :: IORef (MS.Map String String) -> Core -> IO Core
renamer names core = case core of
  Var k -> do
    k' <- genName names k
    return $ Var k'
  Lam x f -> do
    x' <- genName names x
    f' <- renamer names f
    return $ Lam x' f'
  Let m k v f -> do
    k' <- genName names k
    v' <- renamer names v
    f' <- renamer names f
    return $ Let m k' v' f'
  App f x -> do
    f' <- renamer names f
    x' <- renamer names x
    return $ App f' x'
  Sup l a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Sup l a' b'
  Dup l x y v f -> do
    x' <- genName names x
    y' <- genName names y
    v' <- renamer names v
    f' <- renamer names f
    return $ Dup l x' y' v' f'
  Ctr k xs -> do
    xs' <- mapM (renamer names) xs
    return $ Ctr k xs'
  Mat k v m ks -> do
    v'  <- renamer names v
    m'  <- forM m $ \ (k,v) -> do v' <- renamer names v; return (k,v')
    ks' <- forM ks $ \ (c,vs,t) -> do t' <- renamer names t; return (c,vs,t')
    return $ Mat k v' m' ks'
  Op2 o a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Op2 o a' b'
  Ref k i xs -> do
    xs' <- mapM (renamer names) xs
    return $ Ref k i xs'
  other -> 
    return other

genName :: IORef (MS.Map String String) -> String -> IO String
genName names name =
  atomicModifyIORef' names $ \map ->
    case MS.lookup name map of
      Just val -> (map, val)
      Nothing  ->
        let new  = showName (MS.size map)
            map' = MS.insert name new map
        in (map', new)

instance Show Core where
  show = showCore . rename
