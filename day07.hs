import Data.Char
import Data.List
import qualified Data.Map as M

type Step = Char
type Requirements = M.Map Step [Step]

parseInput :: String -> Requirements
parseInput = foldr parseLine M.empty . lines
    where
        parseLine :: String -> Requirements -> Requirements
        parseLine line = recordPre . insertPre
            where pre = line !! 5
                  step = line !! 36
                  insertPre = M.insertWith (insert . head) step [pre]
                  recordPre = M.insertWith (flip const) pre []

available :: Requirements -> [Step]
available = M.keys . M.filter (=="")

remove :: Step -> Requirements -> Requirements
remove s = fmap (delete s) . M.delete s

removeAll :: Requirements -> [Step] -> Requirements
removeAll = foldl (flip remove)

time :: Step -> Int
time s = ord s - ord 'A' + 61

fill :: Int -> [a] -> [a] -> [a]
fill size src dest
    | diff > 0  = dest ++ (take diff src)
    | otherwise = dest
    where diff = size - length dest

solve1 :: Requirements -> [Step]
solve1 = reverse . go ""
    where
        go :: [Step] -> Requirements -> [Step]
        go acc rs
            | not $ null a = let step = head a
                             in go (step:acc) (remove step rs)
            | otherwise    = acc
            where a = available rs

solve2 :: Requirements -> Int
solve2 = go 0 []
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
    requirements <- parseInput <$> readFile "input/07.txt"
    putStrLn . ("Part 1: "++) . solve1 $ requirements
    putStrLn . ("Part 2: "++) . show . solve2 $ requirements
