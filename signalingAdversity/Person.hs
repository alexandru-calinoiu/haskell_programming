module Person where

type Name = String
type Age = Integer

type ValidatePerson a =
    Either [PersonInvalid] a

data PersonInvalid = NameEmpty
                    | AgeTooLow
                    deriving (Eq, Show)

data Person =
    Person { name :: Name, age :: Age }
    deriving (Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age > 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _ (Left badAge)              = Left badAge
