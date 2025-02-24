module HVML.Type where

import Data.Map.Strict as MS
import Data.Word
import Foreign.Ptr

-- Core Types
-- ----------

--show--
data Core
  = Var String -- x
  | Ref String Word64 [Core] -- @fn
  | Era -- *
  | Lam String Core -- λx(F)
  | App Core Core -- (f x)
  | Sup Word64 Core Core -- &L{a b}
  | Dup Word64 String String Core Core -- ! &L{a b} = v body
  | Ctr String [Core] -- #Ctr{a b ...}
  | Mat Core [(String,Core)] [(String,[String],Core)] -- ~ v { #A{a b ...}: ... #B{a b ...}: ... ... }
  | U32 Word32 -- 123
  | Chr Char -- 'a'
  | Op2 Oper Core Core -- (+ a b)
  | Let Mode String Core Core -- ! x = v body
  deriving (Show, Eq)

--show--
data Mode
  = LAZY
  | STRI
  | PARA
  deriving (Show, Eq, Enum)

--show--
data MatchType
  = Switch
  | Match
  | IfLet
  deriving (Show, Eq, Enum)

--show--
data Oper
  = OP_ADD | OP_SUB | OP_MUL | OP_DIV
  | OP_MOD | OP_EQ  | OP_NE  | OP_LT
  | OP_GT  | OP_LTE | OP_GTE | OP_AND
  | OP_OR  | OP_XOR | OP_LSH | OP_RSH
  deriving (Show, Eq, Enum)

--show--
-- A top-level function, including:
-- - copy: true when ref-copy mode is enabled
-- - args: a list of (isArgStrict, argName) pairs
-- - core: the function's body
-- Note: ref-copy improves C speed, but increases interaction count
type Func = ((Bool, [(Bool,String)]), Core)

data Book = Book
  { fidToFun :: MS.Map Word64 Func -- function id to Function object
  , fidToLab :: MS.Map Word64 (MS.Map Word64 ()) -- function id to dup labels used in its body
  , fidToNam :: MS.Map Word64 String -- function id to name
  , namToFid :: MS.Map String Word64 -- function name to id
  , cidToAri :: MS.Map Word64 Word64 -- constructor id to field count (arity)
  , cidToLen :: MS.Map Word64 Word64 -- constructor id to cases length (ADT constructor count)
  , cidToCtr :: MS.Map Word64 String -- constructor id to name
  , ctrToCid :: MS.Map String Word64 -- constructor name to id
  , cidToADT :: MS.Map Word64 Word64 -- constructor id to ADT id (first cid of its datatype)
  } deriving (Show, Eq)

-- Runtime Types
-- -------------

--show--
type Tag  = Word64
type Lab  = Word64
type Loc  = Word64
type Term = Word64

--show--
data TAG
  = DP0
  | DP1
  | VAR
  | ERA
  | APP
  | LAM
  | SUP
  | SUB
  | REF
  | LET
  | CTR
  | MAT
  | IFL
  | SWI
  | W32
  | CHR
  | OPX
  | OPY
  deriving (Eq, Show)

--show--
type HVM = IO

--show--
type ReduceAt = Book -> Loc -> Bool -> HVM Term

-- Constants
-- ---------

--show--
tagT :: Tag -> TAG
tagT 0x00 = DP0
tagT 0x01 = DP1
tagT 0x02 = VAR
tagT 0x03 = SUB
tagT 0x04 = REF
tagT 0x05 = LET
tagT 0x06 = APP
tagT 0x08 = MAT
tagT 0x09 = IFL
tagT 0x0A = SWI
tagT 0x0B = OPX
tagT 0x0C = OPY
tagT 0x0D = ERA
tagT 0x0E = LAM
tagT 0x0F = SUP
tagT 0x10 = CTR
tagT 0x11 = W32
tagT 0x12 = CHR
tagT tag  = error $ "unknown tag: " ++ show tag

_DP0_ :: Tag
_DP0_ = 0x00

_DP1_ :: Tag
_DP1_ = 0x01

_VAR_ :: Tag
_VAR_ = 0x02

_SUB_ :: Tag
_SUB_ = 0x03

_REF_ :: Tag
_REF_ = 0x04

_LET_ :: Tag
_LET_ = 0x05

_APP_ :: Tag
_APP_ = 0x06

_MAT_ :: Tag
_MAT_ = 0x08

_IFL_ :: Tag
_IFL_ = 0x09

_SWI_ :: Tag
_SWI_ = 0x0A

_OPX_ :: Tag
_OPX_ = 0x0B

_OPY_ :: Tag
_OPY_ = 0x0C

_ERA_ :: Tag
_ERA_ = 0x0D

_LAM_ :: Tag
_LAM_ = 0x0E

_SUP_ :: Tag
_SUP_ = 0x0F

_CTR_ :: Tag
_CTR_ = 0x10

_W32_ :: Tag
_W32_ = 0x11

_CHR_ :: Tag
_CHR_ = 0x12

--show--
modeT :: Lab -> Mode
modeT 0x00 = LAZY
modeT 0x01 = STRI
modeT 0x02 = PARA
modeT mode = error $ "unknown mode: " ++ show mode

-- Primitive Functions
_DUP_F_ :: Lab
_DUP_F_ = 0xFFFF

_SUP_F_ :: Lab
_SUP_F_ = 0xFFFE

_LOG_F_ :: Lab
_LOG_F_ = 0xFFFD

primitives :: [(String, Lab)]
primitives =
  [ ("SUP", _SUP_F_)
  , ("DUP", _DUP_F_)
  , ("LOG", _LOG_F_)
  ]

-- Utils
-- -----

-- Getter function for maps
-- TODO: add the type annotatino for mget
mget :: (Ord k, Show k) => MS.Map k a -> k -> a
mget map key =
  case MS.lookup key map of
    Just val -> val
    Nothing  -> error $ "key not found: " ++ show key

-- Returns the first constructor ID in a pattern-match
matFirstCid :: Book -> Core -> Word64
matFirstCid book (Mat _ _ ((ctr,_,_):_)) =
  case MS.lookup ctr (ctrToCid book) of
    Just cid -> cid
    Nothing  -> 0
matFirstCid _ _ = 0

matType :: Book -> Core -> MatchType
matType book (Mat _ _ css) =
  case css of
    ((ctr,_,_):_) | ctr == "0"         -> Switch
    [(ctr,_,_),("_",_,_)]              -> IfLet
    cs | all (\(c,_,_) -> c /= "_") cs -> Match
    _                                  -> error "invalid match"
matType _ _ = error "not a match"

funArity :: Book -> Word64 -> Word64
funArity book fid
  | fid == _SUP_F_ = 3
  | fid == _DUP_F_ = 3
  | fid == _LOG_F_ = 1
  | otherwise = case MS.lookup fid (fidToFun book) of
      Just ((_, args), _) -> fromIntegral (length args)
      Nothing -> error $ "Function ID not found: " ++ show fid
