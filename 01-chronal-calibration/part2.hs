import Data.IntSet (IntSet, member, insert, empty)
import System.IO

readInt :: String -> Int
readInt ('+':xs) = read xs
readInt xs = read xs

findFreq :: [Int] -> Int
findFreq = go empty 0 . cycle
    where
        go :: IntSet -> Int -> [Int] -> Int
        go sums s (int:ints)
            | s `member` sums = s
            | otherwise       = go (insert s sums) (s + int) ints

main = do
    input <- readFile "input.txt"
    print . findFreq $ map readInt (lines input)
