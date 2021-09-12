module FearfulSymmetry where

myWords :: String -> [String]
myWords = split ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentence = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = split '\n'

split :: Char -> String -> [String]
split c s
    | s == "" = []
    | otherwise = first : split c rest
        where first = takeWhile (/=c) s
              rest = drop 1 $ dropWhile (/=c) s

shouldEqual =
    [
    "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main =
    print $
    "Are they equal? "
    ++ show (myLines sentence == shouldEqual)
