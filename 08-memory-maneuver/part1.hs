import Data.Tree (flatten)
import Common

main = do
    tree <- parseInput <$> readFile "input.txt"
    print . sum . concat . flatten $ tree
