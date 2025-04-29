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

-- NOTE:
-- We're currently refactoring HVM3's Core type.

-- OLD TYPE:

-- data Core
  -- = Var Name                    -- x
  -- | Ref Name Word16 [Core]      -- @fn
  -- | Era                         -- *
  -- | Sup Lab Core Core           -- &L{a b}
  -- | Dup Lab Name Name Core Core -- ! &L{a b} = v body
  -- | Set                         -- Set
  -- | All Core Core               -- ∀A.B
  -- | Lam Name Core               -- λx(F)
  -- | App Core Core               -- (f x)
  -- | Adt Name [Core]             -- #Typ{a b ...}
  -- | Ctr Name [Core]             -- #Ctr{a b ...}
  -- | U32                         -- U32
  -- | W32 Word32                  -- 123
  -- | Chr Char                    -- 'a'
  -- | Op2 Oper Core Core          -- (+ a b)
  -- | Let LetT Name Core Core     -- ! x = v body
  -- | Mat MatT Core [Move] [Case] -- ~ v !moves { cases }
  -- deriving (Eq)

-- NEW TYPE:

type FID
  = Word16

data Core
  = Var Name                    -- x
  | Ref Name FID                -- @fun(a,b...)
  | Let LetT Name Core Core     -- !x=v;f
  | Era                         -- *
  | Sup Lab Core Core           -- &L{a,b}
  | Dup Lab Name Name Core Core -- !&L{a,b}=v;f
  | Set                         -- Set
  | Emp                         -- ⊥
  | Efq                         -- λ{}
  | Uni                         -- ⊤
  | Nil                         -- ()
  | Use Core                    -- λ{():f}
  | Sig Core Core               -- ΣA.B
  | Tup Core Core               -- (a,b)
  | Get Name Name Core Core     -- λ{(,):f}
  | All Core Core               -- ΠA.B
  | Lam Name Core               -- λx.F
  | App Core Core               -- (f x)
  | U32                         -- U32
  | W32 Word32                  -- 123
  | Chr Char                    -- 'a'
  | Swi Core [Move] [Case]      -- ~v!m{0:z;+:s}
  | Op2 Oper Core Core          -- (+ a b)
  deriving (Eq)

-- The changes include:
-- - Addition of Type constructors, like Emp, All, Sig, etc.
-- - Addition of empty: Emp/Efq
-- - Addition of unit: Uni/Nil/Use
-- - Addition of pairs: Sig/Tup/Get (behaves like a 0-label Sup/Dup)
-- - Removal of ADT/CTR, in favor of more primitive types
-- - Removal of MAT - now, there is only numeric SWI (for U32)
-- - For simplicity, now, SWI is now always λ{0:z;+:s} (no multi-number switch)
-- - For simplicity, now, REF has no arguments - a top-level definition is just an expression, not a function anymore
-- Our goal is to update the codebase to reflect this change.
-- INSTRUCTIONS:
-- When asked to fill below a commented-out code, you MUST:
-- 1. Rewrite the commented-out code completely, updating it.
-- 2. Keep the old cases (like Lam, App) verbatim - DON'T CHANGE THEM.
-- 3. Implement all the new / missing cases (like Tup, Sig, etc.)
-- 4. Make sure the cases follows the SAME order as the Core type.
-- When asked to implement a missing interaction, you must:
-- 1. Reason about the interaction rule. This is hard. Get it right.
-- 2. Write the missing interaction, including comments.
-- For example, the missing DUP-ALL interaction is:
-- !&L{x0,x1}=∀A.B
-- ----------------- DUP-ALL
-- !&L{A0,A1}=A
-- !&L{B0,B1}=B
-- &L{∀A0.B0,∀A1.B1}
-- IMPORTANT NOTE ON BINDERS:
-- The following terms bind variables:
-- - `λx.f`: a lambda binds 'x' in its body, 'f'
-- - `!x=v;f`; a let binds 'x' in its body, 'f'
-- The following terms DO NOT bind variables:
-- - `λ{}`
-- - `λ{():f}`
-- - `λ{(,):f}`
-- - `λ{0:z;+:s}`
-- - `ΣA.B`
-- - `ΠA.B`
-- Instead, we use lambdas *inside* them. For example:
-- @id  = λ{0:0;+:λp.(+ 1 @id(p))}
-- @fst = λ{(,):λa.λb.a}

data Oper
  = OP_ADD | OP_SUB | OP_MUL | OP_DIV
  | OP_MOD | OP_EQ  | OP_NE  | OP_LT
  | OP_GT  | OP_LTE | OP_GTE | OP_AND
  | OP_OR  | OP_XOR | OP_LSH | OP_RSH
  deriving (Eq, Enum)

-- -- A top-level function, including:
-- -- - copy: true when ref-copy mode is enabled
-- -- - args: a list of (isArgStrict, argName) pairs
-- -- - core: the function's body
-- -- Note: ref-copy improves C speed, but increases interaction count
-- type Func = ((Bool, [(Bool,String)]), Core)

-- A top-level function, including:
-- - copy: true when ref-copy mode is enabled
-- - core: the function's body
-- Note: ref-copy improves C speed, but increases interaction count
type Func = (Bool, Core)

-- Set of labels in a function's body
type HasLab = (MS.Map Lab ())

-- data Book = Book
  -- { fidToFun :: MS.Map Word16 Func   -- func id to Func object
  -- , fidToLab :: MS.Map Word16 HasLab -- func id to dup labels used
  -- , fidToNam :: MS.Map Word16 Name   -- func id to name
  -- , namToFid :: MS.Map Name   Word16 -- func name to id
  -- , cidToAri :: MS.Map Word16 Word16 -- ctor id to field count (arity)
  -- , cidToLen :: MS.Map Word16 Word16 -- ctor id to cases length (ADT ctors)
  -- , cidToCtr :: MS.Map Word16 Name   -- ctor id to name
  -- , ctrToCid :: MS.Map Name   Word16 -- ctor name to id
  -- , cidToADT :: MS.Map Word16 Word16 -- ctor id to ADT id (first ADT cid)
  -- , freshLab :: Lab                  -- auto dup label counter
  -- } deriving (Show, Eq)

data Book = Book
  { fidToFun :: MS.Map FID  Func   -- func id to Func object
  , fidToLab :: MS.Map FID  HasLab -- func id to dup labels used
  , fidToNam :: MS.Map FID  Name   -- func id to name
  , namToFid :: MS.Map Name FID    -- func name to id
  , freshLab :: Lab                -- auto dup label counter
  } deriving (Show, Eq)

-- Runtime Types
-- -------------

type HVM = IO
type ReduceAt = Book -> Loc -> HVM Term

-- Constants
-- ---------

-- -- Tags
-- _DP0_ = 0x00 :: Tag
-- _DP1_ = 0x01 :: Tag
-- _VAR_ = 0x02 :: Tag
-- _FWD_ = 0x03 :: Tag
-- _REF_ = 0x04 :: Tag
-- _LET_ = 0x05 :: Tag
-- _APP_ = 0x06 :: Tag
-- _MAT_ = 0x08 :: Tag
-- _IFL_ = 0x09 :: Tag
-- _SWI_ = 0x0A :: Tag
-- _OPX_ = 0x0B :: Tag
-- _OPY_ = 0x0C :: Tag
-- _ERA_ = 0x0D :: Tag
-- _LAM_ = 0x0E :: Tag
-- _SUP_ = 0x0F :: Tag
-- _CTR_ = 0x10 :: Tag
-- _W32_ = 0x11 :: Tag
-- _CHR_ = 0x12 :: Tag

-- Tags
_DP0_ = 0x00 :: Tag
_DP1_ = 0x01 :: Tag
_VAR_ = 0x02 :: Tag
_REF_ = 0x03 :: Tag
_LET_ = 0x04 :: Tag
_ERA_ = 0x05 :: Tag
_SUP_ = 0x06 :: Tag
_DUP_ = 0x07 :: Tag
_SET_ = 0x08 :: Tag
_EMP_ = 0x09 :: Tag
_EFQ_ = 0x0A :: Tag
_UNI_ = 0x0B :: Tag
_NIL_ = 0x0C :: Tag
_USE_ = 0x0D :: Tag
_SIG_ = 0x0E :: Tag
_TUP_ = 0x0F :: Tag
_GET_ = 0x10 :: Tag
_ALL_ = 0x11 :: Tag
_LAM_ = 0x12 :: Tag
_APP_ = 0x13 :: Tag
_U32_ = 0x14 :: Tag
_W32_ = 0x15 :: Tag
_CHR_ = 0x16 :: Tag
_SWI_ = 0x17 :: Tag
_OP2_ = 0x18 :: Tag

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
mget :: (Ord k, Show k) => MS.Map k a -> k -> a
mget map key =
  case MS.lookup key map of
    Just val -> val
    Nothing  -> error $ "key not found: " ++ show key

-- funArity :: Book -> Word16 -> Word16
-- funArity book fid
  -- | fid == fromIntegral _SUP_F_ = 3
  -- | fid == fromIntegral _DUP_F_ = 3
  -- | fid == fromIntegral _LOG_F_ = 1
  -- | otherwise = case MS.lookup fid (fidToFun book) of
      -- Just ((_, args), _) -> fromIntegral (length args)
      -- Nothing -> error $ "Function ID not found: " ++ show fid

-- NOTE: now, all functions have arity 0.
-- we must rewrite the primitive SUP/DUP/LOG functions to accept lambdas instead.

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

-- showTag :: Tag -> String
-- showTag tag
  -- | tag == _DP0_ = "DP0"
  -- | tag == _DP1_ = "DP1"
  -- | tag == _VAR_ = "VAR"
  -- | tag == _FWD_ = "FWD"
  -- | tag == _REF_ = "REF"
  -- | tag == _LET_ = "LET"
  -- | tag == _APP_ = "APP"
  -- | tag == _MAT_ = "MAT"
  -- | tag == _IFL_ = "IFL"
  -- | tag == _SWI_ = "SWI"
  -- | tag == _OPX_ = "OPX"
  -- | tag == _OPY_ = "OPY"
  -- | tag == _ERA_ = "ERA"
  -- | tag == _LAM_ = "LAM"
  -- | tag == _SUP_ = "SUP"
  -- | tag == _CTR_ = "CTR"
  -- | tag == _W32_ = "W32"
  -- | tag == _CHR_ = "CHR"
  -- | otherwise    = error $ "unknown tag: " ++ show tag

showTag :: Tag -> String
showTag tag
  | tag == _DP0_ = "DP0"
  | tag == _DP1_ = "DP1"
  | tag == _VAR_ = "VAR"
  | tag == _REF_ = "REF"
  | tag == _LET_ = "LET"
  | tag == _ERA_ = "ERA"
  | tag == _SUP_ = "SUP"
  | tag == _DUP_ = "DUP"
  | tag == _SET_ = "SET"
  | tag == _EMP_ = "EMP"
  | tag == _EFQ_ = "EFQ"
  | tag == _UNI_ = "UNI"
  | tag == _NIL_ = "NIL"
  | tag == _USE_ = "USE"
  | tag == _SIG_ = "SIG"
  | tag == _TUP_ = "TUP"
  | tag == _GET_ = "GET"
  | tag == _ALL_ = "ALL"
  | tag == _LAM_ = "LAM"
  | tag == _APP_ = "APP"
  | tag == _U32_ = "U32"
  | tag == _W32_ = "W32"
  | tag == _CHR_ = "CHR"
  | tag == _SWI_ = "SWI"
  | tag == _OP2_ = "OP2"
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

-- showCore :: Core -> String
-- showCore core = maybe (format core) id (sugar core) where

  -- sugar :: Core -> Maybe String
  -- sugar core = nil core <|> str core <|> lst core where
    -- nil :: Core -> Maybe String
    -- nil (Ctr "#Nil" []) = Just "[]"
    -- nil _               = Nothing
    -- str :: Core -> Maybe String
    -- str (Ctr "#Nil" []) = Just "\"\""
    -- str (Ctr "#Cons" [Chr h, t]) = do
      -- rest <- str t
      -- return $ "\"" ++ h : tail rest
    -- str _ = Nothing
    -- lst :: Core -> Maybe String
    -- lst (Ctr "#Nil" [])       = Just "[]"
    -- lst (Ctr "#Cons" [x, xs]) = do
      -- rest <- lst xs
      -- return $ "[" ++ showCore x ++ if rest == "[]" then "]" else " " ++ tail rest
    -- lst _ = Nothing

  -- format :: Core -> String
  -- format (Var k) =
    -- k
  -- format Era =
    -- "*"
  -- format (Lam x f) =
    -- let f' = showCore f in
    -- concat ["λ", x, " ", f']
  -- format (App f x) =
    -- let f' = showCore f in
    -- let x' = showCore x in
    -- concat ["(", f', " ", x', ")"]
  -- format (Sup l a b) =
    -- let a' = showCore a in
    -- let b' = showCore b in
    -- concat ["&", show l, "{", a', " ", b', "}"]
  -- format (Dup l x y v f) =
    -- let v' = showCore v in
    -- let f' = showCore f in
    -- concat ["! &", show l, "{", x, " ", y, "} = ", v', "\n", f']
  -- format (Ref k i xs) =
    -- let xs' = intercalate " " (map showCore xs) in
    -- concat ["@", k, "(", xs', ")"]
  -- format (Ctr k xs) =
    -- let xs' = unwords (map showCore xs) in
    -- concat [k, "{", xs', "}"]
  -- format (Mat k v m ks) =
    -- let v'  = showCore v in
    -- let m'  = concatMap (\(k,v) -> concat [" !", k, "=", showCore v]) m in
    -- let ks' = unwords [concat [c, ":", showCore b] | (c, _, b) <- ks] in
    -- concat ["(~", v', m', " {", ks', "})"]
  -- format (W32 v) =
    -- show v
  -- format (Chr v) =
    -- concat ["'", [v], "'"]
  -- format (Op2 o a b) =
    -- let a' = showCore a in
    -- let b' = showCore b in
    -- concat ["(", show o, " ", a', " ", b', ")"]
  -- format (Let m k v f)
    -- | k == "" =
      -- let v' = showCore v in
      -- let f' = showCore f in
      -- concat [v', "\n", f']
    -- | otherwise =
      -- let v' = showCore v in
      -- let f' = showCore f in
      -- concat ["! ", show m, k, " = ", v', "\n", f']

-- NOTE: we'll remove sugaring for now

showCore :: Core -> String
showCore core = format core where
  format :: Core -> String
  format (Var k) =
    k
  format (Ref k i) =
    concat ["@", k]
  format (Let m k v f) =
    let v' = showCore v in
    let f' = showCore f in
    concat ["!", show m, k, "=", v', ";", f']
  format Era =
    "*"
  format (Sup l a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["&", show l, "{", a', ",", b', "}"]
  format (Dup l x y v f) =
    let v' = showCore v in
    let f' = showCore f in
    concat ["!&", show l, "{", x, ",", y, "}=", v', ";", f']
  format Set =
    "Set"
  format Emp =
    "⊥"
  format Efq =
    "λ{}"
  format Uni =
    "⊤"
  format Nil =
    "()"
  format (Use f) =
    let f' = showCore f in
    concat ["λ{():", f', "}"]
  format (Sig a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["Σ", a', ".", b']
  format (Tup a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["(", a', ",", b', ")"]
  format (Get x y f g) =
    let f' = showCore f in
    concat ["λ{(", x, ",", y, "):", f', "}"]
  format (All a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["Π", a', ".", b']
  format (Lam x f) =
    let f' = showCore f in
    concat ["λ", x, ".", f']
  format (App f x) =
    let f' = showCore f in
    let x' = showCore x in
    concat ["(", f', " ", x', ")"]
  format U32 =
    "U32"
  format (W32 v) =
    show v
  format (Chr v) =
    concat ["'", [v], "'"]
  format (Swi v m ks) =
    let v'  = showCore v in
    let m'  = concatMap (\(k,v) -> concat ["!", k, "=", showCore v, ";"]) m in
    let ks' = intercalate ";" [concat [c, ":", showCore b] | (c, _, b) <- ks] in
    concat ["~", v', m', "{", ks', "}"]
  format (Op2 o a b) =
    let a' = showCore a in
    let b' = showCore b in
    concat ["(", show o, " ", a', " ", b', ")"]

renamer :: IORef (MS.Map String String) -> Core -> IO Core
renamer names core = case core of
  Var k -> do
    k' <- genName names k
    return $ Var k'
  Ref k i -> do
    return $ Ref k i
  Let m k v f -> do
    k' <- genName names k
    v' <- renamer names v
    f' <- renamer names f
    return $ Let m k' v' f'
  Era -> 
    return Era
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
  Set -> 
    return Set
  Emp -> 
    return Emp
  Efq -> 
    return Efq
  Uni -> 
    return Uni
  Nil -> 
    return Nil
  Use f -> do
    f' <- renamer names f
    return $ Use f'
  Sig a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Sig a' b'
  Tup a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Tup a' b'
  Get x y f g -> do
    x' <- genName names x
    y' <- genName names y
    f' <- renamer names f
    g' <- renamer names g
    return $ Get x' y' f' g'
  All a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ All a' b'
  Lam x f -> do
    x' <- genName names x
    f' <- renamer names f
    return $ Lam x' f'
  App f x -> do
    f' <- renamer names f
    x' <- renamer names x
    return $ App f' x'
  U32 -> 
    return U32
  W32 v -> 
    return $ W32 v
  Chr c -> 
    return $ Chr c
  Swi v m ks -> do
    v'  <- renamer names v
    m'  <- forM m $ \ (k,v) -> do v' <- renamer names v; return (k,v')
    ks' <- forM ks $ \ (c,vs,t) -> do t' <- renamer names t; return (c,vs,t')
    return $ Swi v' m' ks'
  Op2 o a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Op2 o a' b'

rename :: Core -> Core
rename core = unsafePerformIO $ do
  names <- newIORef MS.empty
  renamer names core

-- renamer :: IORef (MS.Map String String) -> Core -> IO Core
-- renamer names core = case core of
  -- Var k -> do
    -- k' <- genName names k
    -- return $ Var k'
  -- Ref k i xs -> do
    -- xs' <- mapM (renamer names) xs
    -- return $ Ref k i xs'
  -- Lam x f -> do
    -- x' <- genName names x
    -- f' <- renamer names f
    -- return $ Lam x' f'
  -- Let m k v f -> do
    -- k' <- genName names k
    -- v' <- renamer names v
    -- f' <- renamer names f
    -- return $ Let m k' v' f'
  -- App f x -> do
    -- f' <- renamer names f
    -- x' <- renamer names x
    -- return $ App f' x'
  -- Sup l a b -> do
    -- a' <- renamer names a
    -- b' <- renamer names b
    -- return $ Sup l a' b'
  -- Dup l x y v f -> do
    -- x' <- genName names x
    -- y' <- genName names y
    -- v' <- renamer names v
    -- f' <- renamer names f
    -- return $ Dup l x' y' v' f'
  -- Ctr k xs -> do
    -- xs' <- mapM (renamer names) xs
    -- return $ Ctr k xs'
  -- Mat k v m ks -> do
    -- v'  <- renamer names v
    -- m'  <- forM m $ \ (k,v) -> do v' <- renamer names v; return (k,v')
    -- ks' <- forM ks $ \ (c,vs,t) -> do t' <- renamer names t; return (c,vs,t')
    -- return $ Mat k v' m' ks'
  -- Op2 o a b -> do
    -- a' <- renamer names a
    -- b' <- renamer names b
    -- return $ Op2 o a' b'
  -- other -> 
    -- return other

-- TODO: rewrite renamer to include the mi

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
