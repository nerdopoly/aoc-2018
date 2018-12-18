import Data.Tree

parseInput :: String -> Tree [Int]
parseInput = head . fst . readTrees 1 . map read . words
    where
        readTrees :: Int -> [Int] -> ([Tree [Int]],[Int])
        readTrees 0 xs = ([],xs)
        readTrees t (c:m:xs) = let (children,ys) = readTrees c xs
                                   (metadata,zs) = splitAt m ys
                                   (siblings,remainder) = readTrees (t - 1) zs
                               in ((Node metadata children):siblings,remainder)

value :: Tree [Int] -> Int
value (Node metadata []) = sum metadata
value (Node metadata children) = let indices = map pred . filter (<=length children) . filter (>0)
                                 in sum . map (value . (children !!)) . indices $ metadata

main = do
    tree <- parseInput <$> readFile "input/08.txt"
    putStrLn . ("Part 1: "++) . show . sum . concat . flatten $ tree
    putStrLn . ("Part 2: "++) . show . value $ tree
