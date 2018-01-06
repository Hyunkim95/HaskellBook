data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Woot else x
