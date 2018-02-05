module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

stringify [] = ""
stringify [a] = a
stringify (a:b) = a ++ "-" ++ stringify b

wordNumber :: Int -> String
wordNumber n = stringify $ map digitToWord . digits $ n
