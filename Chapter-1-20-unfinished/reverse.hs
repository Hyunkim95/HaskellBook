module Reverse where

rvrs :: String -> String
rvrs x = drop 8 x ++ " " ++ take 8 x

main :: IO ()
main = print (rvrs("Curry is awesome"))
