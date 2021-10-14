module MyArbitrary where

import           Test.QuickCheck           (Arbitrary, Gen, oneof, sample)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

data Trivial =
    Trivial
    deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

newtype Identity a =
    Identity a
    deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

main :: IO ()
main = sample trivialGen

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

sumGenEqualInt :: Gen (Sum Int Char)
sumGenEqualInt = sumGenEqual
