module Validation where

import           Test.QuickCheck          (Arbitrary (arbitrary), frequency)
import           Test.QuickCheck.Checkers (EqProp ((=-=)), eq, quickBatch)
import           Test.QuickCheck.Classes  (applicative)

data Validation e a =
    Failure e
    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
      frequency [ (1, Failure <$> arbitrary)
                , (1, Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Success f) (Success x) = Success (f x)
    (<*>) (Success _) (Failure y) = Failure y
    (<*>) (Failure x) (Success _) = Failure x
    (<*>) (Failure x) (Failure y) = Failure (x <> y)

type ValidationTyped = (Validation String (String, String, Int))
trigger :: ValidationTyped
trigger = undefined

main :: IO ()
main = do
    quickBatch $ applicative trigger
