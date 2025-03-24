-- //./Type.hs//

module Show where

import Control.Applicative ((<|>))
import Data.Char (chr, ord)
import Data.Char (intToDigit)
import Data.IORef
import Data.List
import Data.Word
import Foreign
import Type
import Numeric (showIntAtBase)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as MS

-- Core Stringification
-- --------------------

showCore :: Core -> String
showCore = coreToString . prettyRename

coreToString :: Core -> String
coreToString core =

  case pretty core of
    Just str -> str
    Nothing -> case core of

      Var nam ->
        nam

      Era ->
        "*"

      Lam lab vr0 bod ->
        let bod' = coreToString bod in
        if lab == 0 then
          "λ" ++ vr0 ++ " " ++ bod'
        else
          "&" ++ show lab ++ "λ" ++ vr0 ++ " " ++ bod'

      App lab fun arg ->
        let fun' = coreToString fun in
        let arg' = coreToString arg in
        if lab == 0 then
          "(" ++ fun' ++ " " ++ arg' ++ ")"
        else
          "&" ++ show lab ++ "(" ++ fun' ++ " " ++ arg' ++ ")"

      Sup lab tm0 tm1 ->
        let tm0' = coreToString tm0 in
        let tm1' = coreToString tm1 in
        "&" ++ show lab ++ "{" ++ tm0' ++ " " ++ tm1' ++ "}"

      Dup lab dp0 dp1 val bod ->
        let val' = coreToString val in
        let bod' = coreToString bod in
        "! &" ++ show lab ++ "{" ++ dp0 ++ " " ++ dp1 ++ "} = " ++ val' ++ "\n" ++ bod'

      Ref nam fid arg ->
        let arg' = intercalate " " (map coreToString arg) in
        "@" ++ nam ++ "(" ++ arg' ++ ")"

      Ctr nam fds ->
        let fds' = unwords (map coreToString fds) in
        nam ++ "{" ++ fds' ++ "}"

      Mat val mov css ->
        let val' = coreToString val in
        let mov' = concatMap (\ (k,v) -> " !" ++ k ++ "=" ++ coreToString v) mov in
        let css' = unwords [ctr ++ ":" ++ coreToString bod | (ctr, fds, bod) <- css] in
        "(~" ++ val' ++ mov' ++ " {" ++ css' ++ "})"

      U32 val ->
        show val

      Chr val ->
        "'" ++ [val] ++ "'"

      Op2 opr nm0 nm1 ->
        let nm0' = coreToString nm0 in
        let nm1' = coreToString nm1 in
        "(" ++ operToString opr ++ " " ++ nm0' ++ " " ++ nm1' ++ ")"

      Let mod nam val bod ->
        if nam == "" then
          let val' = coreToString val in
          let bod' = coreToString bod in
          val' ++ "\n" ++ bod'
        else
          let val' = coreToString val in
          let bod' = coreToString bod in
          "! " ++ modeToString mod ++ nam ++ " = " ++ val' ++ "\n" ++ bod'

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

labToString :: Lab -> String
labToString lab = padLeft (showHex (fromIntegral lab)) 6 '0'

locToString :: Loc -> String
locToString loc = padLeft (showHex (fromIntegral loc)) 8 '0'

termToString :: Term -> String
termToString term =
  let tag = tagToString (termTag term)
      lab = labToString (termLab term)
      loc = locToString (termLoc term)
  in "term_new(" ++ tag ++ ",0x" ++ lab ++ ",0x" ++ loc ++ ")"

-- Pretty Renaming
-- ---------------

prettyRename :: Core -> Core
prettyRename core = unsafePerformIO $ do
  namesRef <- newIORef MS.empty
  go namesRef core
  where

    go namesRef core = case core of

      Var name -> do
        name' <- genName namesRef name
        return $ Var name'

      Lam lab name body -> do
        name' <- genName namesRef name
        body' <- go namesRef body
        return $ Lam lab name' body'

      Let mode name val body -> do
        name' <- genName namesRef name
        val' <- go namesRef val
        body' <- go namesRef body
        return $ Let mode name' val' body'

      App lab fun arg -> do
        fun' <- go namesRef fun
        arg' <- go namesRef arg
        return $ App lab fun' arg'

      Sup lab x y -> do
        x' <- go namesRef x
        y' <- go namesRef y
        return $ Sup lab x' y'

      Dup lab x y val body -> do
        x' <- genName namesRef x
        y' <- genName namesRef y
        val' <- go namesRef val
        body' <- go namesRef body
        return $ Dup lab x' y' val' body'

      Ctr nam args -> do
        args' <- mapM (go namesRef) args
        return $ Ctr nam args'

      Mat val mov css -> do
        val' <- go namesRef val
        mov' <- mapM (\(k,v) -> do v' <- go namesRef v; return (k,v')) mov
        css' <- mapM (\(c,vs,t) -> do t' <- go namesRef t; return (c,vs,t')) css
        return $ Mat val' mov' css'

      Op2 op x y -> do
        x' <- go namesRef x
        y' <- go namesRef y
        return $ Op2 op x' y'

      Ref name fid args -> do
        args' <- mapM (go namesRef) args
        return $ Ref name fid args'

      other -> return other

    genName namesRef name = do
      nameMap <- readIORef namesRef
      case MS.lookup name nameMap of
        Just name' -> return name'
        Nothing -> do
          let newName = genNameFromIndex (MS.size nameMap)
          modifyIORef' namesRef (MS.insert name newName)
          return newName

    genNameFromIndex n = go (n + 1) "" where
      go n ac | n == 0    = ac
              | otherwise = go q (chr (ord 'a' + r) : ac)
              where (q,r) = quotRem (n - 1) 26

-- Pretty Printers
-- ---------------

pretty :: Core -> Maybe String
pretty core = prettyNil core <|> prettyStr core <|> prettyLst core

prettyNil :: Core -> Maybe String
prettyNil (Ctr "#Nil" []) = Just "[]"
prettyNil _               = Nothing

prettyStr :: Core -> Maybe String
prettyStr (Ctr "#Nil" []) = Just "\"\""
prettyStr (Ctr "#Cons" [Chr h, t]) = do
  rest <- prettyStr t
  return $ "\"" ++ h : tail rest
prettyStr _ = Nothing

prettyLst :: Core -> Maybe String
prettyLst (Ctr "#Nil" []) = Just "[]"
prettyLst (Ctr "#Cons" [x, xs]) = do
  rest <- prettyLst xs
  return $ "[" ++ coreToString x ++ if rest == "[]" then "]" else " " ++ tail rest
prettyLst _ = Nothing

-- Dumping
-- -------

dumpHeapRange :: Loc -> Loc -> HVM [(Loc, Term)]
dumpHeapRange ini len =
  if ini < len then do
    head <- got ini
    tail <- dumpHeapRange (ini + 1) len
    if head == 0
      then return tail
      else return ((ini, head) : tail)
  else return []

dumpHeap :: HVM ([(Loc, Term)], Word64)
dumpHeap = do
  len <- getLen
  itr <- getItr
  terms <- dumpHeapRange 0 (fromIntegral len)
  return (terms, itr)

heapToString :: ([(Loc, Term)], Word64) -> String
heapToString (terms, itr) = 
  "set_itr(0x" ++ padLeft (showHex itr) 8 '0' ++ ");\n" ++
  foldr (\(k,v) txt ->
    let addr = padLeft (showHex (fromIntegral k)) 8 '0'
        term = termToString v
    in "set(0x" ++ addr ++ ", " ++ term ++ ");\n" ++ txt) "" terms

padLeft :: String -> Int -> Char -> String
padLeft str n c = replicate (n - length str) c ++ str

showHex :: Word64 -> String
showHex x = showIntAtBase 16 intToDigit (fromIntegral x) ""
