module HVML.Type where

import Data.Map.Strict as MS
import Data.Word
import Foreign.Ptr

-- Core Types
-- ----------

data Core
  = Var String -- x
  | Ref String Word64 [Core] -- @fn
  | Era -- *
  | Lam String Core -- λx(F)
  | App Core Core -- (f x)
  | Sup Word64 Core Core -- &L{a b}
  | Dup Word64 String String Core Core -- ! &L{a b} = v body
  | Ctr Word64 [Core] -- #Ctr{a b ...}
  | Mat Core [(String,Core)] [(String,[String],Core)] -- ~ v { #A{a b ...}: ... #B{a b ...}: ... ... }
  | U32 Word32 -- 123
  | Chr Char -- 'a'
  | Op2 Oper Core Core -- (+ a b)
  | Let Mode String Core Core -- ! x = v body
  deriving (Show, Eq)

data Mode
  = LAZY
  | STRI
  | PARA
  deriving (Show, Eq, Enum)

data Oper
  = OP_ADD | OP_SUB | OP_MUL | OP_DIV
  | OP_MOD | OP_EQ  | OP_NE  | OP_LT
  | OP_GT  | OP_LTE | OP_GTE | OP_AND
  | OP_OR  | OP_XOR | OP_LSH | OP_RSH
  deriving (Show, Eq, Enum)

-- A top-level function, including:
-- - copy: true when ref-copy mode is enabled
-- - args: a list of (isArgStrict, argName) pairs
-- - core: the function's body
-- Note: ref-copy improves C speed, but increases interaction count
type Func = ((Bool, [(Bool,String)]), Core)

-- NOTE: the new idToLabs field is a map from a function id to a set of all
-- DUP/SUP labels used in its body. note that, when a function uses either
-- HVM.SUP or HVM.DUP internally, this field is set to Nothing. this will be
-- used to apply the fast DUP-REF and REF-SUP interactions, when safe to do so
data Book = Book
  { idToFunc :: MS.Map Word64 Func
  , idToName :: MS.Map Word64 String
  , idToLabs :: MS.Map Word64 (MS.Map Word64 ())
  , nameToId :: MS.Map String Word64
  , ctrToAri :: MS.Map String Int
  , ctrToCid :: MS.Map String Word64
  } deriving (Show, Eq)

-- Runtime Types
-- -------------

type Tag  = Word64
type Lab  = Word64
type Loc  = Word64
type Term = Word64

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
  | W32
  | CHR
  | OPX
  | OPY
  deriving (Eq, Show)

type HVM = IO
type TID = Word8

type ReduceAt = Book -> TID -> Loc -> HVM Term

-- C Functions
-- -----------

foreign import ccall unsafe "Runtime.c hvm_init"
  hvmInit :: IO ()

foreign import ccall unsafe "Runtime.c hvm_free"
  hvmFree :: IO ()

foreign import ccall unsafe "Runtime.c alloc_node"
  allocNode :: TID -> Word64 -> IO Word64

foreign import ccall unsafe "Runtime.c set"
  set :: Word64 -> Term -> IO ()

foreign import ccall unsafe "Runtime.c got"
  got :: Word64 -> IO Term

foreign import ccall unsafe "Runtime.c take"
  take :: Word64 -> IO Term

foreign import ccall unsafe "Runtime.c swap"
  swap :: Word64 -> IO Term

foreign import ccall unsafe "Runtime.c term_new"
  termNew :: Tag -> Lab -> Loc -> Term

foreign import ccall unsafe "Runtime.c term_tag"
  termTag :: Term -> Tag

foreign import ccall unsafe "Runtime.c term_get_bit"
  termGetBit :: Term -> Tag

foreign import ccall unsafe "Runtime.c term_lab"
  termLab :: Term -> Lab

foreign import ccall unsafe "Runtime.c term_loc"
  termLoc :: Term -> Loc

foreign import ccall unsafe "Runtime.c term_set_bit"
  termSetBit :: Term -> Tag

foreign import ccall unsafe "Runtime.c term_rem_bit"
  termRemBit :: Term -> Tag

foreign import ccall unsafe "Runtime.c get_len"
  getLen :: IO Word64

foreign import ccall unsafe "Runtime.c get_itr"
  getItr :: IO Word64

foreign import ccall unsafe "Runtime.c inc_itr"
  incItr :: TID -> Word64 -> IO Word64

foreign import ccall unsafe "Runtime.c reduce"
  reduceC :: Word8 -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_let"
  reduceLet :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_era"
  reduceAppEra :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_lam"
  reduceAppLam :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_sup"
  reduceAppSup :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_ctr"
  reduceAppCtr :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_w32"
  reduceAppW32 :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_era"
  reduceDupEra :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_lam"
  reduceDupLam :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_sup"
  reduceDupSup :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_ctr"
  reduceDupCtr :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_w32"
  reduceDupW32 :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_ref"
  reduceDupRef :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_era"
  reduceMatEra :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_lam"
  reduceMatLam :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_sup"
  reduceMatSup :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_ctr"
  reduceMatCtr :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_w32"
  reduceMatW32 :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_era"
  reduceOpxEra :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_lam"
  reduceOpxLam :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_sup"
  reduceOpxSup :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_ctr"
  reduceOpxCtr :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_w32"
  reduceOpxW32 :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_era"
  reduceOpyEra :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_lam"
  reduceOpyLam :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_sup"
  reduceOpySup :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_ctr"
  reduceOpyCtr :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_w32"
  reduceOpyW32 :: Word8 -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_ref_sup"
  reduceRefSup :: Word8 -> Term -> Word64 -> IO Term

foreign import ccall unsafe "Runtime.c hvm_define"
  hvmDefine :: Word64 -> FunPtr (IO Term) -> IO ()

foreign import ccall unsafe "Runtime.c hvm_get_state"
  hvmGetState :: IO (Ptr ())

foreign import ccall unsafe "Runtime.c hvm_set_state"
  hvmSetState :: Ptr () -> IO ()

foreign import ccall unsafe "Runtime.c u12v2_new"
  u12v2New :: Word64 -> Word64 -> Word64

foreign import ccall unsafe "Runtime.c u12v2_x"
  u12v2X :: Word64 -> Word64

foreign import ccall unsafe "Runtime.c u12v2_y"
  u12v2Y :: Word64 -> Word64

-- Constants
-- ---------

tagT :: Tag -> TAG
tagT 0x00 = DP0
tagT 0x01 = DP1
tagT 0x02 = VAR
tagT 0x03 = SUB
tagT 0x04 = REF
tagT 0x05 = LET
tagT 0x06 = APP
tagT 0x08 = MAT
tagT 0x09 = OPX
tagT 0x0A = OPY
tagT 0x0B = ERA
tagT 0x0C = LAM
tagT 0x0D = SUP
tagT 0x0F = CTR
tagT 0x10 = W32
tagT 0x11 = CHR
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

_OPX_ :: Tag
_OPX_ = 0x09

_OPY_ :: Tag
_OPY_ = 0x0A

_ERA_ :: Tag
_ERA_ = 0x0B

_LAM_ :: Tag
_LAM_ = 0x0C

_SUP_ :: Tag
_SUP_ = 0x0D

_CTR_ :: Tag
_CTR_ = 0x0F

_W32_ :: Tag
_W32_ = 0x10

_CHR_ :: Tag
_CHR_ = 0x11

modeT :: Lab -> Mode
modeT 0x00 = LAZY
modeT 0x01 = STRI
modeT 0x02 = PARA
modeT mode = error $ "unknown mode: " ++ show mode

-- Primitive Functions
_DUP_F_ :: Lab
_DUP_F_ = 0xFFF

_SUP_F_ :: Lab
_SUP_F_ = 0xFFE

_LOG_F_ :: Lab
_LOG_F_ = 0xFFD

primitives :: [(String, Lab)]
primitives = 
  [ ("SUP", _SUP_F_)
  , ("DUP", _DUP_F_)
  , ("LOG", _LOG_F_)
  ]

-- Utils
-- -----

-- Getter function for maps
mget map key =
  case MS.lookup key map of
    Just val -> val
    Nothing  -> error $ "key not found: " ++ show key

-- The if-let match stores its target ctr id
ifLetLab :: Book -> Core -> Word64
ifLetLab book (Mat _ _ [(ctr,_,_),("_",_,_)]) =
  case MS.lookup ctr (ctrToCid book) of
    Just cid -> 1 + cid
    Nothing  -> 0
ifLetLab book _ = 0
