-- This is the Haskell version of the naive λ-Calculus enumerator, that just
-- generates all BLC strings and attempts one by one in a loop.

{-# LANGUAGE PatternSynonyms #-}

import Control.Monad (forM_, when)
import Data.Bits (testBit)
import System.Exit (exitSuccess)

data Bits = O Bits | I Bits | E deriving Show
data Term = Lam Term | App Term Term | Var Int deriving Show
data HTerm = HLam (HTerm -> HTerm) | HApp HTerm HTerm | HVar Int | HSub HTerm
data Pair a b = Pair a b deriving Show
data Result a r = Result a r deriving Show

-- Prelude
-- -------

bits :: Int -> Int -> Bits
bits 0 _ = E
bits n i
  | testBit i (n-1) = I (bits (n-1) i)
  | otherwise       = O (bits (n-1) i)

-- Parser
-- ------

parseTerm :: Bits -> Maybe (Result Bits Term)
parseTerm (O src) = do
  Result src nat <- parseInt src
  return $ Result src (Var nat)
parseTerm (I src) = case src of
  O src -> do
    Result src bod <- parseTerm src
    return $ Result src (Lam bod)
  I src -> do
    Result src fun <- parseTerm src
    Result src arg <- parseTerm src
    return $ Result src (App fun arg)
  E -> Nothing
parseTerm E = Nothing

parseInt :: Bits -> Maybe (Result Bits Int)
parseInt (O src) = Just $ Result src 0
parseInt (I src) = do
  Result src nat <- parseInt src
  return $ Result src (1 + nat)
parseInt E = Just $ Result E 0

doParseTerm :: Bits -> Maybe Term
doParseTerm src = do
  Result _ term <- parseTerm src
  return term

doParseHTerm :: Bits -> Maybe HTerm
doParseHTerm src = do
  Result _ term <- parseTerm src
  doBindTerm term

-- Binding
-- -------
-- NOTE: since Haskell doesn't have global variables ($x), we'll bind in two passes
-- The first pass just binds all variables
-- The second pass excludes non-affine terms

uses :: Term -> Int -> Int
uses (Lam bod)     idx = uses bod (idx + 1)
uses (App fun arg) idx = uses fun idx + uses arg idx
uses (Var n)       idx = if n == idx then 1 else 0

affine :: Term -> Bool
affine term = go term 0 where
  go (Lam bod)     dep = uses bod 0 <= 1 && go bod (dep + 1)
  go (App fun arg) dep = go fun dep && go arg dep
  go (Var n)       dep = n < dep

doBindTerm :: Term -> Maybe HTerm
doBindTerm term | affine term = Just (bindTerm term [])
doBindTerm term | otherwise   = Nothing

bindTerm :: Term -> [HTerm] -> HTerm
bindTerm (Lam bod)     ctx = HLam $ \x -> bindTerm bod (x : ctx)
bindTerm (App fun arg) ctx = HApp (bindTerm fun ctx) (bindTerm arg ctx)
bindTerm (Var idx)     ctx = get idx ctx

get :: Int -> [HTerm] -> HTerm
get 0 (x:_) = x
get n (_:t) = get (n-1) t
get _ []    = error "*"

-- Stringification
-- ---------------

showBits :: Bits -> String -> String
showBits (O pred) k = '#':'O':'{': showBits pred ('}':k)
showBits (I pred) k = '#':'I':'{': showBits pred ('}':k)
showBits E        k = '#':'E':k

doShowBits :: Bits -> String
doShowBits bits = showBits bits []

showTerm :: HTerm -> Int -> String -> String
showTerm (HVar idx)     dep k = show (dep - idx - 1) ++ k
showTerm (HLam bod)     dep k = 'λ' : showTerm (bod (HVar dep)) (dep+1) k
showTerm (HApp fun arg) dep k = '(' : showTerm fun dep (' ' : showTerm arg dep (')':k))
showTerm (HSub _)       _   _ = error "*"

doShowTerm :: HTerm -> String
doShowTerm term = showTerm term 0 []

-- Equality
-- --------

eq :: HTerm -> HTerm -> Int -> Bool
eq (HLam aBod)      (HLam bBod)      dep = eq (aBod (HVar dep)) (bBod (HVar dep)) (dep+1)
eq (HApp aFun aArg) (HApp bFun bArg) dep = eq aFun bFun dep && eq aArg bArg dep
eq (HVar aIdx)      (HVar bIdx)      _   = aIdx == bIdx
eq _                _                _   = False

-- Evaluation
-- ----------

wnf :: HTerm -> HTerm
wnf (HLam bod)     = HLam bod
wnf (HApp fun arg) = app (wnf fun) arg
wnf (HVar idx)     = HVar idx
wnf (HSub val)     = HSub val

app :: HTerm -> HTerm -> HTerm
app (HLam bod)     x = wnf (bod (wnf x))
app (HApp fun arg) x = HApp (HApp fun arg) x
app (HVar idx)     x = HApp (HVar idx) x
app (HSub val)     x = HApp (HSub val) x

-- Normalization
-- -------------

nf :: HTerm -> HTerm
nf term = case wnf term of
  HLam bod     -> HLam $ \x -> nf (bod (HSub x))
  HApp fun arg -> HApp (nf fun) (nf arg)
  HVar idx     -> HVar idx
  HSub val     -> val

-- Main
-- ----

a :: HTerm
a = HLam $ \t -> HApp (HApp t (HVar 1)) (HVar 2)

b :: HTerm
b = HLam $ \t -> HApp (HApp t (HVar 2)) (HVar 1)

r :: HTerm
r = HLam $ \x -> HApp x (HLam $ \a -> HLam $ \b -> HLam $ \t -> HApp (HApp t b) a)

-- Solve `?x` in `λaλb(?x λt(t a b)) == λaλbλt(t b a)`
main :: IO ()
main = forM_ [0..2^25-1] $ \i -> do
  let bs = bits 25 i
  case doParseHTerm bs of
    Nothing -> do
      return ()
    Just x -> do
      let solved = eq (nf (HApp x a)) b 0
      -- putStrLn $ show bs
      -- putStrLn $ doShowTerm x
      -- putStrLn $ doShowTerm (nf x)
      -- putStrLn $ show solved
      -- putStrLn $ "----------"
      when solved $ do
        putStrLn (doShowTerm x)
        exitSuccess
