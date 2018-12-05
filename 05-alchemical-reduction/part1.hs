import Data.List
import Data.Char

import Common

main = do
    input <- readFile "input.txt"
    let trimmedInput = dropWhileEnd isSpace input
    print . length . reduce $ trimmedInput
