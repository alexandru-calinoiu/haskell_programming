module SemigroupTest where

import           Test.QuickCheck           (Arbitrary, Gen, frequency,
                                            quickCheck)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

semigroupdAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupdAssoc a b c = a <> (b <> c) == (a <> b) <> c

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  (<>) _ _ = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        Identity <$> arbitrary

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity a') = Identity (a <> a')

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        Two a <$> arbitrary

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        Three a b <$> arbitrary

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary :: Gen Bool
        return (BoolConj a)

instance Semigroup BoolConj where
    (<>) (BoolConj a) (BoolConj a') = BoolConj (a && a')

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary :: Gen Bool
        return (BoolDisj a)

instance Semigroup BoolDisj where
    (<>) (BoolDisj a) (BoolDisj a') = BoolDisj (a || a')

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b =
    Fst a
    | Snd b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (Fst a))
                  , (1, return (Snd b))]

instance Semigroup (Or a  b) where
    (<>) (Snd a) _ = Snd a
    (<>) _ (Snd b) = Snd b
    (<>) _ (Fst b) = Fst b

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

main :: IO ()
main = do
    quickCheck (semigroupdAssoc :: TrivialAssoc)
    quickCheck (semigroupdAssoc :: IdentityAssoc)
    quickCheck (semigroupdAssoc :: TwoAssoc)
    quickCheck (semigroupdAssoc :: ThreeAssoc)
    quickCheck (semigroupdAssoc :: BoolConjAssoc)
    quickCheck (semigroupdAssoc :: BoolDisjAssoc)
    quickCheck (semigroupdAssoc :: OrAssoc)
