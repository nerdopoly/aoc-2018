module Common (readInt) where

readInt :: String -> Int
readInt ('+':xs) = read xs
readInt xs = read xs
