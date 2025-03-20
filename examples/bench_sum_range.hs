import Data.Word

data List = Nil | Cons !Word32 List

range :: Word32 -> List -> List
range 0 xs = xs
range n xs = range (n - 1) (Cons (n - 1) xs)

sum' :: List -> Word32 -> Word32
sum' Nil              r = r
sum' (Cons head tail) r = sum' tail (head + r)

main :: IO ()
main = do
  let !a = range 50_000_000 Nil
  print $ sum' a 0
