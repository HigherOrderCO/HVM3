module Foreign where

import Data.Word
import Foreign.Ptr
import Type

foreign import ccall unsafe "Runtime.c hvm_init"
  hvmInit :: IO ()

foreign import ccall unsafe "Runtime.c hvm_free"
  hvmFree :: IO ()

foreign import ccall unsafe "Runtime.c alloc_node"
  allocNode :: Loc -> IO Loc

foreign import ccall unsafe "Runtime.c set"
  set :: Loc -> Term -> IO ()

foreign import ccall unsafe "Runtime.c got"
  got :: Loc -> IO Term

foreign import ccall unsafe "Runtime.c take"
  take :: Loc -> IO Term

foreign import ccall unsafe "Runtime.c swap"
  swap :: Loc -> Term -> IO Term

foreign import ccall unsafe "Runtime.c term_new"
  termNew :: Tag -> Lab -> Loc -> Term

foreign import ccall unsafe "Runtime.c term_tag"
  termTag :: Term -> Tag

foreign import ccall unsafe "Runtime.c term_get_bit"
  termGetBit :: Term -> Word8

foreign import ccall unsafe "Runtime.c term_lab"
  termLab :: Term -> Lab

foreign import ccall unsafe "Runtime.c term_loc"
  termLoc :: Term -> Loc

foreign import ccall unsafe "Runtime.c term_set_bit"
  termSetBit :: Term -> Term

foreign import ccall unsafe "Runtime.c term_rem_bit"
  termRemBit :: Term -> Term

foreign import ccall unsafe "Runtime.c get_len"
  getLen :: IO Word64

foreign import ccall unsafe "Runtime.c get_itr"
  getItr :: IO Word64

foreign import ccall unsafe "Runtime.c inc_itr"
  incItr :: IO Word64

foreign import ccall unsafe "Runtime.c fresh"
  fresh :: IO Word64

foreign import ccall unsafe "Runtime.c reduce"
  reduceC :: Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_at"
  reduceAtC :: Loc -> IO Term

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
  reduceRefSup :: Term -> Word16 -> IO Term

foreign import ccall unsafe "Runtime.c hvm_define"
  hvmDefine :: Word16 -> FunPtr (IO Term) -> IO ()

foreign import ccall unsafe "Runtime.c hvm_get_state"
  hvmGetState :: IO (Ptr ())

foreign import ccall unsafe "Runtime.c hvm_set_state"
  hvmSetState :: Ptr () -> IO ()

foreign import ccall unsafe "Runtime.c hvm_set_cari"
  hvmSetCari :: Word16 -> Word16 -> IO ()

foreign import ccall unsafe "Runtime.c hvm_set_clen"
  hvmSetClen :: Word16 -> Word16 -> IO ()

foreign import ccall unsafe "Runtime.c hvm_set_cadt"
  hvmSetCadt :: Word16 -> Word16 -> IO ()

foreign import ccall unsafe "Runtime.c hvm_set_fari"
  hvmSetFari :: Word16 -> Word16 -> IO ()

-- FFI imports for interaction counts
foreign import ccall unsafe "Runtime.c hvm_get_let_lazy"    hvmGetLetLazy :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_let_stri"    hvmGetLetStri :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_app_era"     hvmGetAppEra :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_app_lam"     hvmGetAppLam :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_app_sup"     hvmGetAppSup :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_dup_era"     hvmGetDupEra :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_dup_lam"     hvmGetDupLam :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_dup_sup_anni" hvmGetDupSupAnni :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_dup_sup_comm" hvmGetDupSupComm :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_dup_ctr"     hvmGetDupCtr :: Word16 -> IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_dup_w32"     hvmGetDupW32 :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_mat_era"     hvmGetMatEra :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_mat_sup"     hvmGetMatSup :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_mat_ctr"     hvmGetMatCtr :: Word16 -> IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_mat_w32"     hvmGetMatW32 :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_opx_era"     hvmGetOpxEra :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_opx_sup"     hvmGetOpxSup :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_opx_w32"     hvmGetOpxW32 :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_opy_era"     hvmGetOpyEra :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_opy_sup"     hvmGetOpySup :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_opy_w32"     hvmGetOpyW32 :: IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_ref_dup"     hvmGetRefDup :: Word16 -> IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_ref_sup"     hvmGetRefSup :: Word16 -> IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_ref_era"     hvmGetRefEra :: Word16 -> IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_ref_f"       hvmGetRefF :: Word16 -> IO Word64
foreign import ccall unsafe "Runtime.c hvm_get_ref_t"       hvmGetRefT :: Word16 -> IO Word64
