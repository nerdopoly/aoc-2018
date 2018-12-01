import Data.IntSet
import System.IO

readInt :: String -> Int
readInt ('+':xs) = read xs
readInt xs = read xs

findFreq :: [Int] -> IntSet -> Int -> Int
findFreq (int:ints) sums s =
    if member s sums then s else findFreq ints (insert s sums) (s + int)

main = do
    input <- readFile "input.txt"
    print $ findFreq (cycle $ Prelude.map readInt (lines input)) empty 0
