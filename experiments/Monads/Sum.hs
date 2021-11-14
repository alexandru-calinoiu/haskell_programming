module Monads.Sum where

import           Test.QuickCheck          (Arbitrary (arbitrary), frequency)
import           Test.QuickCheck.Checkers (EqProp ((=-=)), eq, quickBatch)
import           Test.QuickCheck.Classes  (applicative, functor, monad)

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second x) = Second (f x)
    fmap _ (First x)  = First x

instance Applicative (Sum a) where
    pure = Second
    (First x) <*> _           = First x
    _ <*> (First x)           = First x
    (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
    return = pure
    (Second x) >>= f = f x
    (First x) >>= _  = First x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [ (1, return (First a))
                  , (2, return (Second b))]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: Sum (Int, String, Int) (Int, Int, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
