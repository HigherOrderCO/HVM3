-- //./Type.hs//

module HVML.Show where

import Control.Monad.State
import Data.Char (intToDigit)
import Data.List
import Data.Word
import HVML.Type
import Numeric (showIntAtBase)

-- Core Stringification
-- --------------------

coreToString :: Core -> String
coreToString (Var nam)                 = nam
coreToString Era                       = "*"
coreToString (Lam vr0 bod)             = "λ" ++ vr0 ++ " " ++ coreToString bod
coreToString (App fun arg)             = "(" ++ coreToString fun ++ " " ++ coreToString arg ++ ")"
coreToString (Sup lab tm0 tm1)         = "&" ++ show lab ++ "{" ++ coreToString tm0 ++ " " ++ coreToString tm1 ++ "}"
coreToString (Dup lab dp0 dp1 val bod) = "! &" ++ show lab ++ "{" ++ dp0 ++ " " ++ dp1 ++ "} = " ++ coreToString val ++ "\n" ++ coreToString bod
coreToString (Ref nam fid arg)         = "@" ++ nam ++ "(" ++ intercalate " " (map coreToString arg) ++ ")"
coreToString (Ctr cid fds)             = "#" ++ show cid ++ "{" ++ unwords (map coreToString fds) ++ "}"
coreToString (Mat val css)             = "(~match " ++ coreToString val ++ " {" ++ unwords (map (\ (ar,cs) -> coreToString cs) css) ++ "})"
coreToString (U32 val)                 = show val
coreToString (Op2 opr nm0 nm1)         = "(" ++  operToString opr ++ " " ++ coreToString nm0 ++ " " ++ coreToString nm1 ++ ")"
coreToString (Let mod nam val bod)     = "! " ++ modeToString mod ++ nam ++ " = " ++ coreToString val ++ " " ++ coreToString bod
coreToString (USp lab tm0 tm1)         = "%" ++ show lab ++ "{" ++ coreToString tm0 ++ " " ++ coreToString tm1 ++ "}"
coreToString (UDp lab dp0 val bod)     = "! %" ++ show lab ++ "{" ++ dp0 ++ "} = " ++ coreToString val ++ "\n" ++ coreToString bod

operToString :: Oper -> String
operToString OP_ADD = "+"
operToString OP_SUB = "-"
operToString OP_MUL = "*"
operToString OP_DIV = "/"
operToString OP_MOD = "%"
operToString OP_EQ  = "=="
operToString OP_NE  = "!="
operToString OP_LT  = "<"
operToString OP_GT  = ">"
operToString OP_LTE = "<="
operToString OP_GTE = ">="
operToString OP_AND = "&"
operToString OP_OR  = "|"
operToString OP_XOR = "^"
operToString OP_LSH = "<<"
operToString OP_RSH = ">>"

modeToString LAZY = ""
modeToString STRI = "."
modeToString PARA = "^"

-- Runtime Stringification
-- -----------------------

tagToString :: Tag -> String
tagToString t = show (tagT t)

labToString :: Word64 -> String
labToString loc = padLeft (showHex loc) 6 '0'

locToString :: Word64 -> String
locToString loc = padLeft (showHex loc) 9 '0'

termToString :: Term -> String
termToString term =
  let tag = tagToString (termTag term)
      lab = labToString (termLab term)
      loc = locToString (termLoc term)
  in "term_new(" ++ tag ++ ",0x" ++ lab ++ ",0x" ++ loc ++ ")"

-- Dumping
-- -------

dumpHeapRange :: Word64 -> Word64 -> HVM [(Word64, Term)]
dumpHeapRange ini len =
  if ini < len then do
    head <- got ini
    tail <- dumpHeapRange (ini + 1) len
    if head == 0
      then return tail
      else return ((ini, head) : tail)
  else return []

dumpHeap :: HVM ([(Word64, Term)], Word64)
dumpHeap = do
  len <- getLen
  itr <- getItr
  terms <- dumpHeapRange 0 len
  return (terms, itr)

heapToString :: ([(Word64, Term)], Word64) -> String
heapToString (terms, itr) = 
  "set_itr(0x" ++ padLeft (showHex itr) 9 '0' ++ ");\n" ++
  foldr (\(k,v) txt ->
    let addr = padLeft (showHex k) 9 '0'
        term = termToString v
    in "set(0x" ++ addr ++ ", " ++ term ++ ");\n" ++ txt) "" terms

padLeft :: String -> Int -> Char -> String
padLeft str n c = replicate (n - length str) c ++ str

showHex :: Word64 -> String
showHex x = showIntAtBase 16 intToDigit (fromIntegral x) ""
