module DoesItTypeCheck where

import           Data.List (sort)
data Person = Person Bool

instance Show Person where
    show (Person True)  = "This is a person"
    show (Person False) = "This is not a person"

printPerson :: Person -> IO()
printPerson = print

data Mood = Blah
    | Woot
    deriving (Eq, Show)

settleDown x = if x == Woot
    then Blah
    else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah =
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

instance Ord Papu where
    compare p p' = GT

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud a = a

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

