module Type where

import Data.Map.Strict as MS
import Data.Word
import Foreign.Ptr

-- Core Types
-- ----------

type Tag  = Word8
type Lab  = Word32
type Loc  = Word32
type Term = Word64

type Name = String
type Move = (String,Core)
type Case = (Name, [Name], Core) -- #Ctr{x0 x1...}: fn

data Core
  = Var Name                    -- x
  | Ref Name Word16 [Core]      -- @fn
  | Era                         -- *
  | Lam Name Core               -- λx(F)
  | App Core Core               -- (f x)
  | Sup Lab Core Core           -- &L{a b}
  | Dup Lab Name Name Core Core -- ! &L{a b} = v body
  | Ctr Name [Core]             -- #Ctr{a b ...}
  | Mat Core [Move] [Case]      -- ~ v !moves { cases }
  | U32 Word32                  -- 123
  | Chr Char                    -- 'a'
  | Op2 Oper Core Core          -- (+ a b)
  | Let Mode String Core Core   -- ! x = v body
  deriving (Show, Eq)

data Mode
  = LAZY
  | STRI
  | PARA
  deriving (Show, Eq, Enum)

data MatchType
  = Switch
  | Match
  | IfLet
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

-- Set of labels in a function's body
type HasLab = (MS.Map Lab ())

data Book = Book
  { fidToFun :: MS.Map Word16 Func   -- func id to Func object
  , fidToLab :: MS.Map Word16 HasLab -- func id to dup labels used
  , fidToNam :: MS.Map Word16 String -- func id to name
  , namToFid :: MS.Map String Word16 -- func name to id
  , cidToAri :: MS.Map Word16 Word16 -- ctor id to field count (arity)
  , cidToLen :: MS.Map Word16 Word16 -- ctor id to cases length (ADT ctors)
  , cidToCtr :: MS.Map Word16 String -- ctor id to name
  , ctrToCid :: MS.Map String Word16 -- ctor name to id
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

-- Match Modes
modeT :: Lab -> Mode
modeT 0x00 = LAZY
modeT 0x01 = STRI
modeT 0x02 = PARA
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

-- Returns the first constructor ID in a pattern-match
matFirstCid :: Book -> Core -> Word16
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

funArity :: Book -> Word16 -> Word16
funArity book fid
  | fid == (fromIntegral _SUP_F_) = 3
  | fid == (fromIntegral _DUP_F_) = 3
  | fid == (fromIntegral _LOG_F_) = 1
  | otherwise = case MS.lookup fid (fidToFun book) of
      Just ((_, args), _) -> fromIntegral (length args)
      Nothing -> error $ "Function ID not found: " ++ show fid
