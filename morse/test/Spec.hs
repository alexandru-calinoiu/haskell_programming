import qualified Data.Map        as M
import           Lib             (Morse, charToMorse, letterToMorse,
                                  morseToChar)
import           Test.QuickCheck (Gen, Property, elements, forAll, quickCheck)

main :: IO ()
main = quickCheck prop_thereAndBackAgain

allowedChar :: [Char]
allowedChar = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChar

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)
