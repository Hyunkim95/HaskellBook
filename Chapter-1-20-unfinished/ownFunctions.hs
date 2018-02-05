module OwnFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = if elem == x then True else myElem elem xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish(xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = (\x -> x) x ++ squishAgain xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:y:xs) = if f x y == GT then myMaximumBy f (x:xs) else myMaximumBy f (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:y:xs) = if f x y == LT then myMinimumBy f (x:xs) else myMinimumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum arr = myMaximumBy compare arr

myMinimum :: (Ord a) => [a] -> a
myMinimum arr = myMinimumBy compare arr
