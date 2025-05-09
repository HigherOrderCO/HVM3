{-./Runtime.c-}

module HVM.Foreign where

import Data.Word
import Foreign.Ptr
import HVM.Type

foreign import ccall "set_len" 
  setLen :: Word64 -> IO ()

foreign import ccall "set_itr"
  setItr :: Word64 -> IO ()

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
  incItr :: IO ()

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

foreign import ccall unsafe "Runtime.c reduce_app_inc"
  reduceAppInc :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_app_dec"
  reduceAppDec :: Term -> Term -> IO Term

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

foreign import ccall unsafe "Runtime.c reduce_dup_inc"
  reduceDupInc :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_dup_dec"
  reduceDupDec :: Term -> Term -> IO Term

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

foreign import ccall unsafe "Runtime.c reduce_mat_inc"
  reduceMatInc :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_mat_dec"
  reduceMatDec :: Term -> Term -> IO Term

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

foreign import ccall unsafe "Runtime.c reduce_opx_inc"
  reduceOpxInc :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opx_dec"
  reduceOpxDec :: Term -> Term -> IO Term

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

foreign import ccall unsafe "Runtime.c reduce_opy_inc"
  reduceOpyInc :: Term -> Term -> IO Term

foreign import ccall unsafe "Runtime.c reduce_opy_dec"
  reduceOpyDec :: Term -> Term -> IO Term

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

showTerm :: Term -> String
showTerm term =
  let tag = showTag (termTag term)
      lab = showLab (termLab term)
      loc = showLoc (termLoc term)
  in "term_new(" ++ tag ++ ",0x" ++ lab ++ ",0x" ++ loc ++ ")"
