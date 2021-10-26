{-# LANGUAGE TupleSections #-}

module SemigroupTest where

import           Test.QuickCheck           (Arbitrary, CoArbitrary, Gen,
                                            Property, forAll, frequency,
                                            quickCheck)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

semigroupdAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupdAssoc a b c = a <> b <> c == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a == a

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  (<>) _ _ = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary =
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

newtype Combine a b =
    Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine u) (Combine u') = Combine (u <> u')

instance (CoArbitrary a, Arbitrary b) => Arbitrary  (Combine a b) where
    arbitrary =
        Combine <$> arbitrary

instance Show (Combine a b) where
    show (Combine _) = "unCombine"

type CombineAssoc = String -> Combine String String -> Combine String String -> Combine String String -> Bool

combineSemigroupAssoc :: (Eq b, Semigroup b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineSemigroupAssoc x a b c = unCombine (a <> b <> c) x == unCombine ((a <> b) <> c) x

genString :: Gen String
genString = arbitrary

prop_combineSemigroupAssoc :: Property
prop_combineSemigroupAssoc = forAll genString (combineSemigroupAssoc :: CombineAssoc)

newtype Comb a =
    Comb { unComp :: a -> a }

instance Arbitrary a => Arbitrary (Comb a) where
    arbitrary = do
        let
            f :: a -> a
            f x = x
        return (Comb f)

instance Semigroup a => Semigroup (Comb a) where
    (<>) (Comb a) (Comb a') = Comb (a <> a')

type CombAssoc = Comb String -> Comb String -> Comb String -> Bool

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (<>) (Success a) _ = Success a
    (<>) _ (Success a) = Success a
    (<>) _ (Failure b) = Failure b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (Success a))
                  , (1, return (Failure b))]

type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

main :: IO ()
main = do
    quickCheck (semigroupdAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

    quickCheck (semigroupdAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)

    quickCheck (semigroupdAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)

    quickCheck (semigroupdAssoc :: ThreeAssoc)
    quickCheck (semigroupdAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

    quickCheck (semigroupdAssoc :: BoolDisjAssoc)
    quickCheck (semigroupdAssoc :: OrAssoc)
    quickCheck (semigroupdAssoc :: ValidationAssoc)

    quickCheck prop_combineSemigroupAssoc

-- Monoid 1

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

-- Monoid 2

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

-- Monoid 3

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

-- Monoid 4

instance Monoid BoolConj where
    mempty = BoolConj True

-- Monoid  6

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

-- Monoid 8

newtype Mem s a =
    Mem {
        runMem :: s -> (a, s)
    }

instance Semigroup a => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = Mem h
        where h s = let
                        (a, s') = f s
                        (a', _) = g s
                    in
                    (a <> a', s')

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (mempty, )
