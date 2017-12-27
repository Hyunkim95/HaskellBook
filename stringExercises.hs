module StringExercises where

appendExclamation x = x ++ "!"

fourthLetter x = x !! 4

dropTwelve x = drop 12 x

thirdLetter :: String -> Char
thirdLetter x = x !! 3

nthLetter :: Int -> Char
nthLetter x = "Curry is awesome!" !! x

rvrs x = drop 8 x ++ " " ++ take 8 x
