module Chapter7 where

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

foldBool :: a -> a -> Bool -> a
foldBool x y z =
  case z of
    True -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z == True = y
  | z == False = x
