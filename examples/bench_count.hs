import Data.Word

count :: Word32 -> Word32 -> Word32
count 0 k = k
count p k = count (p - 1) (k + 1)

main :: IO ()
main = print $ count 2_000_000_000 0
