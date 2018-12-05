module Common (reduce) where

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
