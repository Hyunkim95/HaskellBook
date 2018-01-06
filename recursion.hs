module Recursion where

total :: (Eq a, Num a) => a -> a
total 1 = 1
total x = x + total (x-1)

total2 :: (Eq a, Num a) => a -> a
total2 x
    | x == 1 = 1
    | otherwise = x + total2 (x - 1)

multi :: (Integral a) => a -> a -> a
multi _ 0 = 0
multi x y = x + multi x (y - 1)
