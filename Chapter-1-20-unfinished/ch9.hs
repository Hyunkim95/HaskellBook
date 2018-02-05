module ChapterNine where
import Data.Char

filterUpper :: [Char] -> [Char]
filterUpper str = filter (\x -> isUpper x) str

capitaliseFirst :: [Char] -> [Char]
capitaliseFirst str = map (\x -> if x == head str then toUpper x else x) str

capEverything :: [Char] -> [Char]
capEverything [] = []
capEverything (x:xs) = toUpper x : capEverything xs

capFirstAndReturn :: [Char] -> Char
capFirstAndReturn str = head $ capitaliseFirst str
