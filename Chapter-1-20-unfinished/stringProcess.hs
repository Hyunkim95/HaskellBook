import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe sentence = recurseAndReplaceThe $ words sentence
 where recurseAndReplaceThe ([]) = ""
       recurseAndReplaceThe (x:xs) = if notThe(x) /= Nothing then x ++ " " ++ recurseAndReplaceThe(xs) else recurseAndReplaceThe(xs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = recurseAndCount $ words sentence
recurseAndCount (x:y:[]) = fn x y
  where fn a b = if notThe(a) == Nothing && elem (head b) ['a','e','i','o','u']
               then 1
               else 0
recurseAndCount (x:y:xs) = fn x y + recurseAndCount (y:xs)
  where fn a b = if notThe(a) == Nothing && elem (head b) ['a','e','i','o','u']
               then 1
               else 0

countVowels :: String -> Integer
countVowels (x:[]) = if elem x ['a','e','i','o','u']
                then 1
                else 0
countVowels (x:xs) = if elem x ['a','e','i','o','u']
                     then 1 + countVowels(xs)
                     else 0 + countVowels(xs)

countConsonants (x:[]) = if notElem x ['a','e','i','o','u']
                         then 1
                         else 0
countConsonants (x:xs) = if notElem x ['a','e','i','o','u']
                         then 1 + countVowels(xs)
                         else 0 + countVowels(xs)

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = if countVowels str > countConsonants str
             then Nothing
             else Just (Word' str)

data Nat = Zero | Succ Nat deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger(a)

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just (getNat n)
  where
    getNat 0 = Zero
    getNat n = Succ (getNat $ n - 1)

isJust :: Maybe a -> Bool
isJust (Just _ ) = False
isJust (Nothing) = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee a _ Nothing = a
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just b) = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = (Just x)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just a : xs) = a : catMaybes xs

flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe arr
 | elem Nothing arr = Nothing
 | otherwise = Just (getJust arr)
 where
  getJust [] = []
  getJust (Just x :xs) = x : getJust(xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [v] ++ (myIterate f f x)
