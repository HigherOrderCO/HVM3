-- //./pseudo_metavar_factors.hvml//

import Control.Monad (forM_, when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitSuccess)
import Text.Printf (printf)

data Bin = O Bin | I Bin | E deriving (Show, Eq)

u32 :: Bin -> Int
u32 (O p) = 2 * u32 p + 0
u32 (I p) = 2 * u32 p + 1
u32 E     = 0

bin :: Int -> Int -> Bin
bin 0 _ = E
bin s n = case n `mod` 2 of
  0 -> O (bin (s-1) (n `div` 2))
  _ -> I (bin (s-1) (n `div` 2))

eq :: Bin -> Bin -> Bool
eq E     E     = True
eq (O a) (O b) = eq a b
eq (I a) (I b) = eq a b
eq _     _     = False

inc :: Bin -> Bin
inc (O p) = I p
inc (I p) = O (inc p)
inc E     = E

add :: Bin -> Bin -> Bin
add (O a) (O b) = O (add a b)
add (O a) (I b) = I (add a b)
add (I a) (O b) = I (add a b)
add (I a) (I b) = O (inc (add a b))
add E     b     = E
add a     E     = E

mul :: Bin -> Bin -> Bin
mul _ E     = E
mul a (O b) = O (mul a b)
mul a (I b) = add a (O (mul a b))

cat :: Bin -> Bin -> Bin
cat (O a) b = O (cat a b)
cat (I a) b = I (cat a b)
cat E     b = b

k = 14
h = 15
s = 30
p = I(I(I(O(O(O(I(I(O(I(O(I(O(O(O(O(I(O(I(I(O(O(I(O(O(I(O(I(O(I(E))))))))))))))))))))))))))))))

--INJECT--

main :: IO ()
main = do
  start <- getCurrentTime
  forM_ [0..2^h-1] $ \a -> do
    forM_ [0..2^h-1] $ \b -> do
      let binA = cat (bin h a) (bin h 0)
      let binB = cat (bin h b) (bin h 0)
      when (eq (mul binA binB) p) $ do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ "FACT: " ++ show a ++ " " ++ show b
        putStrLn $ "TIME: " ++ printf "%.7f seconds" (realToFrac duration :: Double)
        exitSuccess
