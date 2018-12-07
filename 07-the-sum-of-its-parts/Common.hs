module Common (Step, Requirements, parseInput, available, remove) where

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
