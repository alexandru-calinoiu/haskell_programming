module Person where

import           Foundation.IO (stdout)
import           System.IO     (BufferMode (NoBuffering), hSetBuffering)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
    | AgeToLow
    | PersonInvalidUnknown String
    deriving Eq

instance Show PersonInvalid where
  show NameEmpty                = "Does not have a name"
  show AgeToLow                 = "Age is to low"
  show (PersonInvalidUnknown s) = "Invalid, " ++ s


mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeToLow
    | otherwise = Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStrLn "Ok, so you want a person, here is what you need to do"
    name <- gimmeName
    age <- gimmeAge
    case mkPerson name age of
        Right person -> putStrLn $ "Yay! Succesfully got a person " ++ show person
        Left invalidPerson -> putStrLn $ "Failure: " ++ show invalidPerson

gimmeName :: IO Name
gimmeName = do
    putStr "Tell me their name: "
    getLine

gimmeAge :: IO Age
gimmeAge = do
    putStr "Tell me their age: "
    age <- getLine
    return $ toInteger (read age :: Int)

