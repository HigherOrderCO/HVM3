module HVML.Foreign where

import Data.Word
import Foreign.Ptr
import HVML.Type

foreign import ccall unsafe "Runtime.c hvm_init"
  hvmInit :: IO ()
foreign import ccall unsafe "Runtime.c hvm_free"
  hvmFree :: IO ()
foreign import ccall unsafe "Runtime.c alloc_node"
  allocNode :: Word64 -> IO Word64
foreign import ccall unsafe "Runtime.c set_old"
  setOld :: Word64 -> Term -> IO ()
foreign import ccall unsafe "Runtime.c set_new"
  setNew :: Word64 -> Term -> IO ()
foreign import ccall unsafe "Runtime.c got"
  got :: Word64 -> IO Term
foreign import ccall unsafe "Runtime.c take"
  take :: Word64 -> IO Term
foreign import ccall unsafe "Runtime.c swap"
  swap :: Word64 -> IO Term
foreign import ccall unsafe "Runtime.c spush"
  spush :: Term -> IO ()
foreign import ccall unsafe "Runtime.c spop"
  spop :: IO Term
foreign import ccall unsafe "Runtime.c rpush"
  rpush :: Term -> Word64 -> IO Word64
foreign import ccall unsafe "Runtime.c rtake"
  rtake :: Word64 -> IO Term
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
foreign import ccall unsafe "Runtime.c term_size"
  term_size :: Term -> Word64
foreign import ccall unsafe "Runtime.c get_len"
  getLen :: IO Word64
foreign import ccall unsafe "Runtime.c get_itr"
  getItr :: IO Word64
foreign import ccall unsafe "Runtime.c inc_itr"
  incItr :: IO Word64
foreign import ccall unsafe "Runtime.c fresh"
  fresh :: IO Word64
foreign import ccall unsafe "Runtime.c reduce"
  reduceC :: Term -> Word64 -> IO Term
foreign import ccall unsafe "Runtime.c reduce_at"
  reduceAtC :: Term -> Word64 -> IO Term
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
foreign import ccall unsafe "Runtime.c reduce_dup_ref"
  reduceDupRef :: Term -> Term -> IO Term
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
foreign import ccall unsafe "Runtime.c reduce_ref_sup"
  reduceRefSup :: Term -> Word64 -> IO Term
foreign import ccall unsafe "Runtime.c hvm_define"
  hvmDefine :: Word64 -> FunPtr (IO Term) -> IO ()
foreign import ccall unsafe "Runtime.c hvm_get_state"
  hvmGetState :: IO (Ptr ())
foreign import ccall unsafe "Runtime.c hvm_set_state"
  hvmSetState :: Ptr () -> IO ()
foreign import ccall unsafe "Runtime.c hvm_set_cari"
  hvmSetCari :: Word64 -> Word16 -> IO ()
foreign import ccall unsafe "Runtime.c hvm_set_clen"
  hvmSetClen :: Word64 -> Word16 -> IO ()
foreign import ccall unsafe "Runtime.c hvm_set_cadt"
  hvmSetCadt :: Word64 -> Word16 -> IO ()
foreign import ccall unsafe "Runtime.c hvm_set_fari"
  hvmSetFari :: Word64 -> Word16 -> IO ()
