module DatabaseProcessing where

import           Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)

data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Show, Ord)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello World"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate time) -> time) . filter isDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map (\(DbNumber v) -> v) . filter isDbNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items = fromIntegral (sumDb items) / fromIntegral (length . filterDbNumber $ items)

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False
