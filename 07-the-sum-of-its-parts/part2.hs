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

workers :: Int
workers = 5

solve :: Requirements -> Int
solve = go 0 []
    where
        go :: Int -> [(Step,Int)] -> Requirements -> Int
        go t jobs rs
            | completed     = t
            | jobsAvailable = go t (fill workers a jobs) rs
            | jobsDone      = go t (jobs \\ done) (removeAll rs . map fst $ done)
            | otherwise     = go (t + 1) (tick jobs) rs
            where a = map init $ (available rs) \\ (map fst jobs)
                  init s = (s,time s)
                  tick = map (\(s,r) -> (s,r - 1))
                  done = filter ((<1) . snd) $ jobs
                  completed = (null rs && null jobs)
                  jobsAvailable = length jobs < workers && not (null a)
                  jobsDone = not . null $ done

main = do
    input <- readFile "input.txt"
    print . solve . parseInput $ input
