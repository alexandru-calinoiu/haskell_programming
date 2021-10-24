module First where

import           Test.QuickCheck (Arbitrary (arbitrary), frequency, quickCheck)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a

newtype First' a =
    First' { getFirst' :: Maybe a }
    deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) (First' x) (First' x') = case (x, x') of
                                    (Just y, Nothing)  -> First' (Just y)
                                    (Nothing, Just y') -> First' (Just y')
                                    (Just y, Just _)   -> First' (Just y)
                                    (Nothing, Nothing) -> First' Nothing

instance Monoid (First' a) where
  mempty = First' Nothing

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
      a <- arbitrary
      frequency [(1, return (First' (Just a)))
                , (1, return (First' Nothing))]

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (ma :: FirstMappend)
    quickCheck (mli :: FstId)
    quickCheck (mri :: FstId)
