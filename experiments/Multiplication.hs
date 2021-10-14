module Multiplication where

import           Test.Hspec      (describe, hspec, it, shouldBe)
import           Test.QuickCheck (Testable (property))

main :: IO ()
main = hspec $ do
    describe "mult`" $ do
        it "will multiple 2 and 2 and return 4" $ do
            mult' 2 2 `shouldBe` (4 :: Integer)
        it "multiplication by 0 is always 0" $ do
            property $ \x -> mult' x 0 == (0 :: Int)

mult' :: Integral a => a -> a -> a
mult' _ 0  = 0
mult' m 1  = m
mult' m m' = m + mult' m (m' - 1)
