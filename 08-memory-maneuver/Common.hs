module Common (parseInput) where

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
