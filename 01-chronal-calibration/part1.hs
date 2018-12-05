import Common

main = do
    input <- readFile "input.txt"
    print . sum $ map readInt (lines input)
