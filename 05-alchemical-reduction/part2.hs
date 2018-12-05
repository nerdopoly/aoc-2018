import Data.List
import Data.Char

import Common

removePolymer :: String -> Char -> String
removePolymer s p = filter (not . (`elem` [toUpper p,toLower p])) s

main = do
    input <- readFile "input.txt"
    let trimmedInput = dropWhileEnd isSpace input
        lengthWithoutPolymer = length . reduce . removePolymer trimmedInput
    print . minimum . map lengthWithoutPolymer $ ['a'..'z']
