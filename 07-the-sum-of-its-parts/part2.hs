import Data.List
import Data.Char

import Common

removeAll :: Requirements -> [Step] -> Requirements
removeAll = foldl (flip remove)

time :: Step -> Int
time s = ord s - ord 'A' + 61

fill :: Int -> [a] -> [a] -> [a]
fill size src dest
    | diff > 0  = dest ++ (take diff src)
    | otherwise = dest
    where diff = size - length dest

solve :: Requirements -> Int
solve = go 0 []
    where
        go :: Int -> [(Step,Int)] -> Requirements -> Int
        go t jobs rs
            | null rs && null jobs            = t
            | length jobs < 5 && not (null a) = go t (fill 5 a jobs) rs
            | not (null done)                 = go t (jobs \\ done) (removeAll rs . map fst $ done)
            | otherwise                       = go (t + 1) (map (\(s,r) -> (s,r - 1)) jobs) rs
            where a = map (\step -> (step,time step)) $ (available rs) \\ (map fst jobs)
                  done = filter ((<1) . snd) $ jobs

main = do
    input <- readFile "input.txt"
    print . solve . parseInput $ input
