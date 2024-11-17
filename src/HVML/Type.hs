-- //./Runtime.c//

module HVML.Type where

import Data.Map.Strict as MS
import Data.Word
import Foreign.Ptr

-- Core Types
-- ----------

data Core
  = Var String
  | Ref String Word64 [Core]
  | Era
  | Lam String Core
  | App Core Core
  | Sup Word64 Core Core
  | Dup Word64 String String Core Core
  | Ctr Word64 [Core]
  | Mat Core [(Int,Core)]
  | U32 Word32
  | Op2 Oper Core Core
  | Let Mode String Core Core
  | USp Word64 Core Core
  | UDp Word64 String Core Core
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

type Func = ([String], Core)

data Book = Book
  { idToFunc :: MS.Map Word64 Func
  , idToName :: MS.Map Word64 String
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
  | UDP
  | APP
  | ERA
  | LAM
  | SUP
  | USP
  | SUB
  | REF
  | LET
  | CTR
  | MAT
  | W32
  | OPX
  | OPY
  deriving (Eq, Show)

type HVM = IO

-- C Functions
-- -----------

foreign import ccall unsafe "Runtime.c hvm_init"
  hvmInit :: IO ()

foreign import ccall unsafe "Runtime.c hvm_free"
  hvmFree :: IO ()

foreign import ccall unsafe "Runtime.c alloc_node"
  allocNode :: Word64 -> IO Word64

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

foreign import ccall unsafe "Runtime.c term_lab"
  termLab :: Term -> Lab

foreign import ccall unsafe "Runtime.c term_loc"
  termLoc :: Term -> Loc

foreign import ccall unsafe "Runtime.c term_key"
  termKey :: Term -> Loc

foreign import ccall unsafe "Runtime.c get_len"
  getLen :: IO Word64

foreign import ccall unsafe "Runtime.c get_itr"
  getItr :: IO Word64

foreign import ccall unsafe "Runtime.c inc_itr"
  incItr :: IO Word64

foreign import ccall unsafe "Runtime.c reduce"
  reduce :: Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_let"
  reduceLet :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_era"
  reduceAppEra :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_lam"
  reduceAppLam :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_sup"
  reduceAppSup :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_ctr"
  reduceAppCtr :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_w32"
  reduceAppW32 :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_usp"
  reduceAppUsp :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_era"
  reduceDupEra :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_lam"
  reduceDupLam :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_sup"
  reduceDupSup :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_ctr"
  reduceDupCtr :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_w32"
  reduceDupW32 :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_usp"
  reduceDupUsp :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_era"
  reduceMatEra :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_lam"
  reduceMatLam :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_sup"
  reduceMatSup :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_ctr"
  reduceMatCtr :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_w32"
  reduceMatW32 :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_usp"
  reduceMatUsp :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_era"
  reduceOpxEra :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_lam"
  reduceOpxLam :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_sup"
  reduceOpxSup :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_ctr"
  reduceOpxCtr :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_w32"
  reduceOpxW32 :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_usp"
  reduceOpxUsp :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_era"
  reduceOpyEra :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_lam"
  reduceOpyLam :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_sup"
  reduceOpySup :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_ctr"
  reduceOpyCtr :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_w32"
  reduceOpyW32 :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_usp"
  reduceOpyUsp :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_udp_era"
  reduceUdpEra :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_udp_lam"
  reduceUdpLam :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_udp_sup"
  reduceUdpSup :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_udp_ctr"
  reduceUdpCtr :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_udp_w32"
  reduceUdpW32 :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_udp_usp"
  reduceUdpUsp :: Term -> Term -> IO Term

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
tagT 0x02 = UDP
tagT 0x03 = VAR
tagT 0x04 = SUB
tagT 0x05 = REF
tagT 0x06 = LET
tagT 0x07 = APP
tagT 0x08 = MAT
tagT 0x09 = OPX
tagT 0x0A = OPY
tagT 0x0B = ERA
tagT 0x0C = LAM
tagT 0x0D = SUP
tagT 0x0E = USP
tagT 0x0F = CTR
tagT 0x10 = W32
tagT tag  = error $ "unknown tag: " ++ show tag

_DP0_, _DP1_, _VAR_, _SUB_, _UDP_, _REF_, _LET_, _APP_, _MAT_, _OPX_, _OPY_, _ERA_, _LAM_, _SUP_, _USP_, _CTR_, _W32_ :: Tag
_DP0_ = 0x00
_DP1_ = 0x01
_UDP_ = 0x02
_VAR_ = 0x03
_SUB_ = 0x04
_REF_ = 0x05
_LET_ = 0x06
_APP_ = 0x07
_MAT_ = 0x08
_OPX_ = 0x09
_OPY_ = 0x0A
_ERA_ = 0x0B
_LAM_ = 0x0C
_SUP_ = 0x0D
_USP_ = 0x0E
_CTR_ = 0x0F
_W32_ = 0x10

modeT :: Lab -> Mode
modeT 0x00 = LAZY
modeT 0x01 = STRI
modeT 0x02 = PARA
modeT mode = error $ "unknown mode: " ++ show mode
