module List where

myEnum :: (Enum a, Eq a) => a -> a -> [a]
myEnum x y = case pred x == y of
  True -> []
  False -> x:myEnum (succ x) y

myEnum2 x y = go x y
  where go start end
          | pred start == y = []
          | otherwise = start : myEnum2 (succ start) end

myWords string = go string
  where go x
          | x == "" = []
          | otherwise = (takeWhile (/=' ') x ) : go (dropWhile (==' ') (dropWhile (/=' ') x ))

splitByChar c str = go c str
  where go c str
          | str == "" = []
          | otherwise = (takeWhile (/=c) str ) : go (dropWhile (==c) (dropWhile (/=c) str ))
