import System.IO

readInt :: String -> Int
readInt ('+':xs) = read xs
readInt xs = read xs

main = do
    input <- readFile "input.txt"
    print $ sum (map readInt (lines input))
