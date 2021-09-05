{-# OPTIONS_GHC -Wall #-}

module RegisteredUser where

newtype Username =
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User =
    UnregisteredUser
    | RegisteredUser Username AccountNumber

instance Show User where
    show UnregisteredUser = "Unregistered user"
    show (RegisteredUser (Username username) (AccountNumber accountNumber)) =
        username ++ " " ++ show accountNumber


