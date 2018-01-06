module Cipher where
import Data.Char

cipher str = map func str
  where func x
          | x == 'w' = 'a'
          | x == 'y' = 'b'
          | x == 'z' = 'c'
          | otherwise = (chr $ ord(x) + 3)
