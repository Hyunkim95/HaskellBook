data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' a) (First' Nada) = (First' a)
  mappend (First' Nada) (First' a) = (First' a)
  mappend (First' a) _ = (First' a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend
