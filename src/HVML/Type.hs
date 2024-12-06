{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module HVML.Type where

import Data.Map.Strict as MS
import Data.Word
import Foreign.Ptr

-- Core Types
-- ----------

data CoreT r
  = VarT String
  | RefT String Word64 [r]
  | EraT
  | LamT String r
  | AppT r r
  | SupT Word64 r r
  | DupT Word64 String String r r
  | CtrT Word64 [r]
  | MatT r [(String,r)] [(String,[String],r)]
  | U32T Word32
  | ChrT Char
  | Op2T Oper r r
  | LetT Mode String r r
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Core = Core { unCore :: CoreT Core }
deriving instance Show Core
deriving instance Eq Core

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
type Func = ((Bool, [(Bool,String)]), Core)

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
  reduceC :: TID -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_let"
  reduceLet :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_era"
  reduceAppEra :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_lam"
  reduceAppLam :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_sup"
  reduceAppSup :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_ctr"
  reduceAppCtr :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_w32"
  reduceAppW32 :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_era"
  reduceDupEra :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_lam"
  reduceDupLam :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_sup"
  reduceDupSup :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_ctr"
  reduceDupCtr :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_w32"
  reduceDupW32 :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_ref"
  reduceDupRef :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_era"
  reduceMatEra :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_lam"
  reduceMatLam :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_sup"
  reduceMatSup :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_ctr"
  reduceMatCtr :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_w32"
  reduceMatW32 :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_era"
  reduceOpxEra :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_lam"
  reduceOpxLam :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_sup"
  reduceOpxSup :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_ctr"
  reduceOpxCtr :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_w32"
  reduceOpxW32 :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_era"
  reduceOpyEra :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_lam"
  reduceOpyLam :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_sup"
  reduceOpySup :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_ctr"
  reduceOpyCtr :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_w32"
  reduceOpyW32 :: TID -> Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_ref_sup"
  reduceRefSup :: TID -> Term -> Word64 -> IO Term

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
tagT 0x07 = MAT
tagT 0x08 = OPX
tagT 0x09 = OPY
tagT 0x0A = ERA
tagT 0x0B = LAM
tagT 0x0C = SUP
tagT 0x0D = CTR
tagT 0x0E = W32
tagT 0x0F = CHR
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
_MAT_ = 0x07

_OPX_ :: Tag
_OPX_ = 0x08

_OPY_ :: Tag
_OPY_ = 0x09

_ERA_ :: Tag
_ERA_ = 0x0A

_LAM_ :: Tag
_LAM_ = 0x0B

_SUP_ :: Tag
_SUP_ = 0x0C

_CTR_ :: Tag
_CTR_ = 0x0D

_W32_ :: Tag
_W32_ = 0x0E

_CHR_ :: Tag
_CHR_ = 0x0F

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

-- Pattern Synonyms
-- ----------------

pattern Var :: String -> Core
pattern Var x = Core (VarT x)

pattern Ref :: String -> Word64 -> [Core] -> Core
pattern Ref a b c = Core (RefT a b c)

pattern Era :: Core
pattern Era = Core EraT

pattern Lam :: String -> Core -> Core
pattern Lam a b = Core (LamT a b)

pattern App :: Core -> Core -> Core
pattern App a b = Core (AppT a b)

pattern Sup :: Word64 -> Core -> Core -> Core
pattern Sup a b c = Core (SupT a b c)

pattern Dup :: Word64 -> String -> String -> Core -> Core -> Core
pattern Dup a b c d e = Core (DupT a b c d e)

pattern Ctr :: Word64 -> [Core] -> Core
pattern Ctr a b = Core (CtrT a b)

pattern Mat :: Core -> [(String,Core)] -> [(String,[String],Core)] -> Core
pattern Mat a b c = Core (MatT a b c)

pattern U32 :: Word32 -> Core
pattern U32 a = Core (U32T a)

pattern Chr :: Char -> Core
pattern Chr a = Core (ChrT a)

pattern Op2 :: Oper -> Core -> Core -> Core
pattern Op2 a b c = Core (Op2T a b c)

pattern Let :: Mode -> String -> Core -> Core -> Core
pattern Let a b c d = Core (LetT a b c d)

{-# COMPLETE Var, Ref, Era, Lam, App, Sup, Dup, Ctr, Mat, U32, Chr, Op2, Let #-}



