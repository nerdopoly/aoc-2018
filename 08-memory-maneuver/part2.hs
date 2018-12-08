import Debug.Trace
import Data.Tree
import Common

value :: Tree [Int] -> Int
value (Node metadata []) = sum metadata
value (Node metadata children) = let indices = map pred . filter (<=length children) . filter (>0)
                                 in sum . map (value . (children !!)) . indices $ metadata

main = do
    tree <- parseInput <$> readFile "input.txt"
    print . value $ tree
