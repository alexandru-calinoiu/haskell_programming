module Monads.EitherMonad where

type Founded = Int

type Coders = Int

data SoftwareShop =
    Shop {
        founded       :: Founded
        , programmers :: Coders
    } deriving (Show, Eq)

data FoundedError =
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 500 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Founded -> Coders -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded' <- validateFounded years
    programmers' <- validateCoders coders
    if programmers' > founded' `div` 10
        then Left $ TooManyCodersForYears founded' programmers'
        else Right $ Shop founded' programmers'
