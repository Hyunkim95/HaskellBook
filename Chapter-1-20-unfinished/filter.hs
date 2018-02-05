module Filter where
threeMultiple :: Integral a => [a] -> [a]
threeMultiple arr = filter (\x -> (rem x 3) == 0) arr

myWords string = go string
  where go x
          | x == "" = []
          | otherwise = (takeWhile (/=' ') x ) : go (dropWhile (==' ') (dropWhile (/=' ') x ))

numberThreeMultiples arr = length . threeMultiple $ arr

removeStuff :: String -> [String]
removeStuff str = filter (\x -> notElem x ["the", "an", "a"]) $ myWords str
