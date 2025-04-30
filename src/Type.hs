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

type FnID = Word16
type Name = String
type Move = (Name, Core) -- !x = term

data LetT = LAZY | STRI deriving (Eq, Enum)
data MatT = SWI | MAT Word16 | IFL Word16 deriving (Show, Eq)

-- **MAIN GOAL: REFACTOR HVM3'S CORE TYPE.**

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

data Core
  = Var Name                    -- x
  | Ref Name FnID [Core]        -- @fun(a,b...)
  | Let LetT Name Core Core     -- !x=v;f
  | Era                         -- *
  | Sup Lab Core Core           -- &L{a,b}
  | Dup Lab Name Name Core Core -- !&L{a,b}=v;f
  | Set                         -- Set
  | Emp                         -- ⊥
  | Efq Core [Move]             -- ~x!ms{}
  | Uni                         -- ⊤
  | Nil                         -- ()
  | Use Core [Move] Core        -- ~x!ms{():f}
  | U32                         -- U32
  | W32 Word32                  -- 12345
  | Swi Core [Move] Core Core   -- ~x!ms{0:z;+:s}
  | Op2 Oper Core Core          -- (+ a b)
  | Sig Core Core               -- ΣA.B
  | Tup Core Core               -- (a,b)
  | Get Core [Move] Core        -- ~x!ms{(,):f}
  | All Core Core               -- ΠA.B
  | Lam Name Core               -- λx.F
  | App Core Core               -- (f x)
  deriving (Eq)

-- The changes include:
-- - Addition of Type-Formers, like Emp, All, Sig, etc.
-- - Addition of empty: Emp/Efq
-- - Addition of unit: Uni/Nil/Use
-- - Addition of pairs: Sig/Tup/Get (behaves like a 0-label Sup/Dup)
-- - Removal of ADT/CTR, in favor of more primitive types
-- - Removal of CHR (which was just another W32)
-- - Removal of the generic MAT, in favor of specific pattern-matches:
-- - - Efq: for the Empty type
-- - - Use: for the Unit type
-- - - Swi: for the U32 type
-- - - Get: for the Sigma type
-- - For simplicity, now, SWI is always ~x{0:z;+:s} (no multi-number switch)
-- - FWD has been removed (it was unused)
-- **Our goal is to update the whole codebase to reflect this change.**
-- 
-- INSTRUCTIONS:
-- 
-- When asked to fill below a commented-out code, you must rewrite it completely, updating it to comply to the changes above.
-- Remove discarded cases (like CTR), keep old cases (like Lam/App) verbatin (important: don't change existing definitions!), include new/missing cases (like Tup).
-- Preserve original comments and everything else. Avoid loss of information.
-- 
-- When asked to implement a missing interaction, reason about it longer, to make sure you get it right.
-- For example, the missing DUP-ALL interaction is:
-- !&L{x0,x1}=∀A.B
-- ----------------- DUP-ALL
-- !&L{A0,A1}=A
-- !&L{B0,B1}=B
-- &L{∀A0.B0,∀A1.B1}
-- Because All doesn't bind a variable.
-- Getting these right demands reasoning.
-- 
-- NOTES:
-- 
-- The following terms bind variables:
-- - `λx.f`: a lambda binds 'x' in its body, 'f'
-- - `!x=v;f`; a let binds 'x' in its body, 'f'
-- The following terms DO NOT bind variables:
-- - `~x{}`
-- - `~x{():f}`
-- - `~x{(,):f}`
-- - `~x{0:z;+:s}`
-- - `ΣA.B`
-- - `ΠA.B`
-- Instead, we use lambdas *inside* them. For example:
-- @id  = λx.~x{0:0;+:λp.(+ 1 @id(p))}
-- @fst = λx.~x{(,):λa.λb.a}

-- Below is the complete list of new elims/intros/types:
-- Eliminators:
-- - APP
-- - USE
-- - GET
-- - SWI
-- - DUP
-- - OPX
-- - OPY
-- Introducers:
-- - ERA
-- - SUP
-- - NIL
-- - W32
-- - TUP
-- - LAM
-- Type-Formers:
-- - SET
-- - EMP
-- - UNI
-- - U32
-- - SIG
-- - ALL

-- An interaction happens when an eliminator meet an introducer or type-former.
-- Below is the complete list of new interactions:
-- Definitions:
-- - REF (calls a top-level function)
-- - LET (substitutes a local definition)
-- Applications:
-- - APP-ERA
-- - APP-SUP
-- - APP-LAM
-- - APP-NIL (err)
-- - APP-TUP (err)
-- - APP-W32 (err)
-- - APP-TYP (err)
-- Uses:
-- - USE-ERA
-- - USE-SUP
-- - USE-LAM (err)
-- - USE-NIL
-- - USE-TUP (err)
-- - USE-W32 (err)
-- - USE-TYP (err)
-- Gets:
-- - GET-ERA
-- - GET-SUP
-- - GET-NIL (err)
-- - GET-TUP
-- - GET-LAM (err)
-- - GET-W32 (err)
-- - GET-TYP (err)
-- Switches:
-- - SWI-ERA
-- - SWI-SUP
-- - SWI-NIL (err)
-- - SWI-TUP (err)
-- - SWI-LAM (err)
-- - SWI-W32
-- - SWI-TYP (err)
-- Duplications:
-- - DUP-ERA
-- - DUP-SUP
-- - DUP-LAM
-- - DUP-NIL
-- - DUP-TUP
-- - DUP-W32
-- - DUP-TYP
-- Operations:
-- - OPX-ERA
-- - OPX-SUP
-- - OPX-NIL
-- - OPX-TUP
-- - OPX-LAM (err)
-- - OPX-W32
-- - OPX-TYP (err)
-- Extra:
-- - DUP-REF-COPY
-- - REF-SUP-COPY
-- For simplicity, we're grouping all type-formers (Emp, Uni, etc.) as 'TYP'.
-- In C, we will implement their interactions in grouped reduce_foo_typ() functions.
-- 
-- STYLE GUIDE:
-- - Always order cases in the SAME order as they were defined on the Core type.
-- - As much as possible, favor SINGLE-LETTER field names, or TWO-LETTER field names (on list-like fields), inspired by the letters used in the comments on the Core type. For example, instead of:
--    For example, instead of `format (Use moves body) = ...`, write `format (Use ms b) = ...`
-- - Since Haskell doesn't accept uppercase variable names, prepend an underscore. Example: `format (All _A _B) = ...`.
-- - Do NOT use the ' (apostrophe) character in variable names. Prefer underscores. Example: `a_` instead of `a'`
-- - In general, attempt to mimic the original coding style as close as possible.
-- - KEEP COMMENTS, ERROR HANDLERS, ETC. FROM THE ORIGINAL CODE - DON'T LOSE INFORMATION

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
  { fidToFun :: MS.Map FnID Func   -- func id to Func object
  , fidToLab :: MS.Map FnID HasLab -- func id to dup labels used
  , fidToNam :: MS.Map FnID Name   -- func id to name
  , namToFid :: MS.Map Name FnID   -- func name to id
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
_U32_ = 0x0E :: Tag
_W32_ = 0x0F :: Tag
_SWI_ = 0x10 :: Tag
_OPX_ = 0x11 :: Tag
_OPY_ = 0x12 :: Tag
_SIG_ = 0x13 :: Tag
_TUP_ = 0x14 :: Tag
_GET_ = 0x15 :: Tag
_ALL_ = 0x16 :: Tag
_LAM_ = 0x17 :: Tag
_APP_ = 0x18 :: Tag

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

funArity :: Book -> Word16 -> Word16
funArity book fid
  | fid == fromIntegral _SUP_F_ = 3
  | fid == fromIntegral _DUP_F_ = 3
  | fid == fromIntegral _LOG_F_ = 1
  | otherwise = case MS.lookup fid (fidToFun book) of
      Just ((_, args), _) ->
        fromIntegral (length args)
      Nothing ->
        error $ "Function ID not found: " ++ show fid

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
  | tag == _U32_ = "U32"
  | tag == _W32_ = "W32"
  | tag == _SWI_ = "SWI"
  | tag == _OPX_ = "OPX"
  | tag == _OPY_ = "OPY"
  | tag == _SIG_ = "SIG"
  | tag == _TUP_ = "TUP"
  | tag == _GET_ = "GET"
  | tag == _ALL_ = "ALL"
  | tag == _LAM_ = "LAM"
  | tag == _APP_ = "APP"
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

-- renamer :: IORef (MS.Map String String) -> Core -> IO Core
-- renamer names core = case core of
  -- Var k -> do
    -- k' <- genName names k
    -- return $ Var k'
  -- Ref k i -> do
    -- return $ Ref k i
  -- Let m k v f -> do
    -- k' <- genName names k
    -- v' <- renamer names v
    -- f' <- renamer names f
    -- return $ Let m k' v' f'
  -- Era -> 
    -- return Era
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
  -- Set -> 
    -- return Set
  -- Emp -> 
    -- return Emp
  -- Efq -> 
    -- return Efq
  -- Uni -> 
    -- return Uni
  -- Nil -> 
    -- return Nil
  -- Use f -> do
    -- f' <- renamer names f
    -- return $ Use f'
  -- Sig a b -> do
    -- a' <- renamer names a
    -- b' <- renamer names b
    -- return $ Sig a' b'
  -- Tup a b -> do
    -- a' <- renamer names a
    -- b' <- renamer names b
    -- return $ Tup a' b'
  -- Get x y f g -> do
    -- x' <- genName names x
    -- y' <- genName names y
    -- f' <- renamer names f
    -- g' <- renamer names g
    -- return $ Get x' y' f' g'
  -- All a b -> do
    -- a' <- renamer names a
    -- b' <- renamer names b
    -- return $ All a' b'
  -- Lam x f -> do
    -- x' <- genName names x
    -- f' <- renamer names f
    -- return $ Lam x' f'
  -- App f x -> do
    -- f' <- renamer names f
    -- x' <- renamer names x
    -- return $ App f' x'
  -- U32 -> 
    -- return U32
  -- W32 v -> 
    -- return $ W32 v
  -- Chr c -> 
    -- return $ Chr c
  -- Swi v m ks -> do
    -- v'  <- renamer names v
    -- m'  <- forM m $ \ (k,v) -> do v' <- renamer names v; return (k,v')
    -- ks' <- forM ks $ \ (c,vs,t) -> do t' <- renamer names t; return (c,vs,t')
    -- return $ Swi v' m' ks'
  -- Op2 o a b -> do
    -- a' <- renamer names a
    -- b' <- renamer names b
    -- return $ Op2 o a' b'

-- rename :: Core -> Core
-- rename core = unsafePerformIO $ do
  -- names <- newIORef MS.empty
  -- renamer names core

-- NOTE: we'll remove sugaring for now

showCore :: Core -> String
showCore = format
  where
    format :: Core -> String
    format (Var k) = k
    format (Ref k i xs) =
      let xs' = intercalate " " (map showCore xs) in
      concat ["@", k, "(", xs', ")"]
    format (Let m k v f) =
      let v' = showCore v in
      let f' = showCore f in
      if k == "" then
        concat [v', "\n", f']
      else
        concat ["! ", show m, k, " = ", v', "\n", f']
    format Era = "*"
    format (Sup l a b) =
      let a' = showCore a in
      let b' = showCore b in
      concat ["&", show l, "{", a', ",", b', "}"]
    format (Dup l x y v f) =
      let v' = showCore v in
      let f' = showCore f in
      concat ["! &", show l, "{", x, ",", y, "} = ", v', "\n", f']
    format Set = "Set"
    format Emp = "⊥"
    format (Efq c ms) =
      let c' = showCore c in
      let ms' = concatMap (\(k,v) -> concat [" !", k, "=", showCore v]) ms in
      concat ["~", c', ms', "{}"]
    format Uni = "⊤"
    format Nil = "()"
    format (Use c ms b) =
      let c' = showCore c in
      let ms' = concatMap (\(k,v) -> concat [" !", k, "=", showCore v]) ms in
      let b' = showCore b in
      concat ["~", c', ms', "{():", b', "}"]
    format U32 = "U32"
    format (W32 v) = show v
    format (Swi c ms z s) =
      let c' = showCore c in
      let ms' = concatMap (\(k,v) -> concat [" !", k, "=", showCore v]) ms in
      let z' = showCore z in
      let s' = showCore s in
      concat ["~", c', ms', "{0:", z', ";+:", s', "}"]
    format (Op2 o a b) =
      let a' = showCore a in
      let b' = showCore b in
      concat ["(", show o, " ", a', " ", b', ")"]
    format (Sig _A _B) =
      let a' = showCore _A in
      let b' = showCore _B in
      concat ["Σ", a', ".", b']
    format (Tup a b) =
      let a' = showCore a in
      let b' = showCore b in
      concat ["(", a', ",", b', ")"]
    format (Get c ms b) =
      let c' = showCore c in
      let ms' = concatMap (\(k,v) -> concat [" !", k, "=", showCore v]) ms in
      let b' = showCore b in
      concat ["~", c', ms', "{(,):", b', "}"]
    format (All _A _B) =
      let a' = showCore _A in
      let b' = showCore _B in
      concat ["Π", a', ".", b']
    format (Lam x f) =
      let f' = showCore f in
      concat ["λ", x, ".", f']
    format (App f x) =
      let f' = showCore f in
      let x' = showCore x in
      concat ["(", f', " ", x', ")"]

renamer :: IORef (MS.Map String String) -> Core -> IO Core
renamer names core = case core of
  Var k -> do
    k' <- genName names k
    return $ Var k'
  Ref k i xs -> do
    xs' <- mapM (renamer names) xs
    return $ Ref k i xs'
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
  Efq c ms -> do
    c' <- renamer names c
    ms' <- forM ms $ \(k,v) -> do v' <- renamer names v; return (k,v')
    return $ Efq c' ms'
  Uni -> 
    return Uni
  Nil -> 
    return Nil
  Use c ms b -> do
    c' <- renamer names c
    ms' <- forM ms $ \(k,v) -> do v' <- renamer names v; return (k,v')
    b' <- renamer names b
    return $ Use c' ms' b'
  U32 -> 
    return U32
  W32 v -> 
    return $ W32 v
  Swi c ms z s -> do
    c' <- renamer names c
    ms' <- forM ms $ \(k,v) -> do v' <- renamer names v; return (k,v')
    z' <- renamer names z
    s' <- renamer names s
    return $ Swi c' ms' z' s'
  Op2 o a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Op2 o a' b'
  Sig _A _B -> do
    _A' <- renamer names _A
    _B' <- renamer names _B
    return $ Sig _A' _B'
  Tup a b -> do
    a' <- renamer names a
    b' <- renamer names b
    return $ Tup a' b'
  Get c ms b -> do
    c' <- renamer names c
    ms' <- forM ms $ \(k,v) -> do v' <- renamer names v; return (k,v')
    b' <- renamer names b
    return $ Get c' ms' b'
  All _A _B -> do
    _A' <- renamer names _A
    _B' <- renamer names _B
    return $ All _A' _B'
  Lam x f -> do
    x' <- genName names x
    f' <- renamer names f
    return $ Lam x' f'
  App f x -> do
    f' <- renamer names f
    x' <- renamer names x
    return $ App f' x'

rename :: Core -> Core
rename core = unsafePerformIO $ do
  names <- newIORef MS.empty
  renamer names core

-- 

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


-- COMPLETE INTERACTION TABLE (updated)

-- @foo(a,b...)
-- ----------------- REF
-- book[foo](a,b...)

-- Let:

-- ! x = v ; f
-- ----------- LET
-- x <- v
-- f

-- (* a)
-- ----- APP-ERA
-- *

-- (λx.f a)
-- -------- APP-LAM
-- x <- a
-- f

-- (&L{f0,f1} a)
-- ------------------- APP-SUP
-- ! &L{a0,a1} = a;
-- &L{(f0 x0),(f1 x1)}

-- (() a)
-- ------ APP-NIL
-- error

-- ((f0,f1) a)
-- ----------- APP-TUP
-- error

-- (123 a)
-- ------- APP-W32
-- error

-- (T a)
-- ----- APP-TYP
-- error

-- ~*{():f}
-- -------- USE-ERA
-- *

-- ~&L{a,b}{():f}
-- ----------------------- USE-SUP
-- ! &L{f0,f1} = f;
-- &L{~a{():f0},~b{():f1}}

-- ~λx.g{():f}
-- ----------- USE-LAM
-- error

-- ~(){():f}
-- --------- USE-NIL
-- f

-- ~(p,q){():f}
-- ------------ USE-TUP
-- error

-- ~123{():f}
-- ---------- USE-W32
-- error

-- ~T{():f}
-- -------- USE-TYP
-- error

-- ~*{(,):f}
-- --------- GET-ERA
-- *

-- ~&L{a,b}{(,): f}
-- ------------------------- GET-SUP
-- ! &L{f0,f1} = f;
-- &L{~a{(,):f0},~b{(,):f1}}

-- ~(){(,):f}
-- ---------- GET-NIL
-- error

-- ~(a,b){(,):f}
-- ------------- GET-TUP
-- (f a b)

-- ~λx.g{(,):f}
-- ------------ GET-LAM
-- error

-- ~123{(,):f}
-- ----------- GET-W32
-- error

-- ~TYP{(,):f}
-- ----------- GET-TYP
-- error

-- ~*{0:z;+:s}
-- ----------- SWI-ERA
-- *

-- ~&L{a,b}{0:z;+:s}
-- ------------------------------- SWI-SUP
-- ! &L{z0,z1} = z;
-- ! &L{s0,s1} = s;
-- &L{~a{0:z0;+:s0},~b{0:z1;+:s1}}

-- ~(){0:z;+:s}
-- ------------ SWI-NIL
-- error

-- ~(a,b){0:z;+:s}
-- --------------- SWI-TUP
-- error

-- ~λx.g{0:z;+:s}
-- -------------- SWI-LAM
-- error

-- ~n{0:z;+:s}
-- ----------- SWI-W32
-- if n = 0:
--   z
-- else:
--   (s (n-1))

-- ~T{0:z;+:s}
-- ----------- SWI-TYP
-- error

-- ! &L{r,s} = *
-- ------------- DUP-ERA
-- r <- *
-- s <- *

-- ! &L{x,y} = &L{a,b}
-- ------------------- DUP-SUP
-- x <- a
-- y <- b

-- ! &L{r,s} = λx.f
-- ---------------- DUP-LAM
-- r <- λx0.f0
-- s <- λx1.f1
-- x <- &L{x0,x1}
-- ! &L{f0,f1} = f

-- ! &L{x,y} = ()
-- -------------- DUP-NIL
-- x <- ()
-- y <- ()

-- ! &L{x,y} = (a,b)
-- ----------------- DUP-TUP
-- ! &L{a0,a1} = a
-- ! &L{b0,b1} = b
-- x <- &L{a0,b0}
-- y <- &L{a1,b1}

-- ! &L{x,y} = 123
-- --------------- DUP-W32
-- x <- 123
-- y <- 123

-- ! &L{x,y} = Set
-- --------------- DUP-SET
-- x <- Set
-- y <- Set

-- ! &L{x,y} = ⊥
-- ------------- DUP-EMP
-- x <- ⊥
-- y <- ⊥

-- ! &L{x,y} = ⊤
-- ------------- DUP-UNI
-- x <- ⊤
-- y <- ⊤

-- ! &L{x,y} = U32
-- -------------- DUP-U32
-- x <- U32
-- y <- U32

-- ! &L{x,y} = ΣA.B
-- ----------------- DUP-SIG
-- ! &L{A0,A1} = A
-- ! &L{B0,B1} = B
-- x <- ΣA0.B0
-- y <- ΣA1.B1

-- ! &L{x,y} = ΠA.B
-- ----------------- DUP-ALL
-- ! &L{A0,A1} = A
-- ! &L{B0,B1} = B
-- x <- ΠA0.B0
-- y <- ΠA1.B1

-- (<op * y)
-- --------- OPX-ERA
-- *

-- (<op λx.f y)
-- ----------- OPX-LAM
-- error

-- (<op &L{a,b} y)
-- ------------------------- OPX-SUP
-- ! &L{y0,y1} = y;
-- &L{(<op a y0),(<op b y1)}

-- (<op () y)
-- ---------- OPX-NIL
-- error

-- (<op (x0,x1) y)
-- --------------- OPX-TUP
-- error

-- (<op x y)
-- --------- OPX-W32
-- (>op y x)

-- (<op T y)
-- --------- OPX-TYP
-- error

-- (>op x *)
-- --------- OPY-ERA
-- *

-- (>op x λy.f)
-- ------------ OPY-LAM
-- error

-- (>op x &L{y0,y1})
-- ----------------------- OPY-SUP
-- &L{>op(x y0),>op(x y1)}

-- (>op x ())
-- ---------- OPY-NIL
-- error

-- (>op x (y0,y1))
-- --------------- OPY-TUP
-- error

-- (>op x y)
-- --------- OPY-W32
-- x <op> y

-- (>op x T)
-- --------- OPY-TYP
-- error

-- ! &L{x,y} = @foo(a,b...)
-- ------------------------ DUP-REF-COPY (when &L not in @foo)
-- ! &L{a0,a1} = a
-- ! &L{b0,b1} = b
-- ...
-- x <- @foo(a0,b0...)
-- y <- @foo(a1,b1...)

-- @foo(&L{ax,ay},b,c...)
-- ---------------------- REF-SUP-COPY (when @L not in @foo)
-- ! &L{bx,by} = b
-- ! &L{cx,by} = c
-- ...
-- &L{@foo(ax,bx,cx...),@foo(ay,by,cy,...)}
