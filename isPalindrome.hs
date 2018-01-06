module IsPalindrome where

isPalindrome :: (Eq a) => String -> Bool
isPalindrome x = x == reverse x
