module Addition where

import           Test.Hspec      (describe, hspec, it, shouldBe)
import           Test.QuickCheck (Testable (property))

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1 :: Integer) `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` (4 :: Integer)
        it "x + 1 is always greater then x" $ do
            property $ \x -> x + 1 > (x :: Int)

