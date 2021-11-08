module BadMonoid where

import           Data.Monoid               ()
import           Test.QuickCheck           (Arbitrary, frequency)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (2, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

instance EqProp Bull where
    (=-=) = eq
