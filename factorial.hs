import Data.Monoid
import Data.Foldable
import Control.Monad.Writer
import Control.Monad.State

fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
    let n' = n-1
    tell $ Sum 1
    m <- fact2 n'
    let r = n*m
    tell $ Sum 1
    return r

ex1 = runWriter (fact1 10)
