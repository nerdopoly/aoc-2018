import Data.List
import Data.Char

groupPairs :: String -> [String]
groupPairs (x:y:ys)
    | (toUpper x == toUpper y) && (x /= y) = [x,y] : groupPairs ys
    | otherwise                            = [x] : groupPairs (y:ys)
groupPairs (x:[]) = [x]:[]
groupPairs [] = []

reduce :: String -> String
reduce s
    | reducible = reduce . concat . filter (\x -> length x == 1) $ pairs
    | otherwise     = s
    where pairs = groupPairs s
          reducible = any (>1) . map length $ pairs

removePolymer :: String -> Char -> String
removePolymer s p = filter (not . (`elem` [toUpper p,toLower p])) s

reducedLength :: String -> Int
reducedLength = length . reduce

main = do
    input <- dropWhileEnd isSpace <$> readFile "input/05.txt"
    putStrLn . ("Part 1: "++) . show . reducedLength $ input
    putStrLn . ("Part 2: "++) . show . minimum . map reducedLength $ removePolymer input <$> ['a'..'z']
