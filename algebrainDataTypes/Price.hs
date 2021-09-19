module Price where

newtype Price = Price Integer deriving (Eq, Show)

newtype Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data AirLine =
    PapuAir
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
    | Plane AirLine Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar :: Vehicle
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man
