data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
mempty = Nada
mappend (Only a) (Only b) = Only $ mappend a b
mappend Nada (Only a) = (Only a)
mappend (Only a) Nada = (Only a)
