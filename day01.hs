import Data.IntSet (IntSet, member, insert, empty)

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
    ints <- fmap readInt . lines <$> readFile "input/01.txt"
    putStrLn . ("Part 1: "++) . show . sum $ ints
    putStrLn . ("Part 2: "++) . show . findFreq $ ints
