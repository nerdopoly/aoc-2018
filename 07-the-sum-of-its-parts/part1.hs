import Common

solve :: Requirements -> [Step]
solve = reverse . go ""
    where
        go :: [Step] -> Requirements -> [Step]
        go acc rs
            | not $ null a = let step = head a
                             in go (step:acc) (remove step rs)
            | otherwise    = acc
            where a = available rs

main = do
    input <- readFile "input.txt"
    putStrLn . solve . parseInput $ input
