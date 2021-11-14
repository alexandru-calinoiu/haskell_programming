module Monads.Cow where

data Cow = Cow {
    name     :: String
    , age    :: Int
    , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative x
    | x >= 0 = Just x
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
    namey <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow namey agey weighty)
