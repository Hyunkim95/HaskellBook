import Data.Time

data DataBaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)
theDatabase :: [DataBaseItem]

theDatabase =
  [
    DbDate(UTCTime (fromGregorian 1911 5 1)(secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1)(secondsToDiffTime 34123))
  ]

filterDbDate :: [DataBaseItem] -> [UTCTime]
filterDbDate arr = foldr getTime [] arr
  where
    getTime (DbDate time) dates = time:dates
    getTime _ dates = dates

filterDbNumber :: [DataBaseItem] -> [Integer]
filterDbNumber = foldr getNumber []
  where
    getNumber (DbNumber number) dates = number:dates
    getNumber _ dates = dates

mostRecent :: [DataBaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DataBaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DataBaseItem] -> Double
avgDb arr = total / entries
  where total = (fromIntegral . sumDb) arr
        entries = (fromIntegral . length . filterDbNumber) arr
