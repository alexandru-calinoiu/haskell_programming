module Phone where

import           Data.Char  (chr, ord)
import           Data.Int   ()
import           Data.List  (elemIndex, find, sort)
import           Data.Maybe (fromMaybe)

newtype DaPhone = DaPhone [(Digit, String )] deriving Show

convo :: [String]
convo =
    ["Wanna play 20 questions"
    , "Ya"
    , "U 1sh haha"
    , "Lol OK. Have u ever tasted alcohol"]

type Digit = Char
type Presses = Int

makePhone :: DaPhone
makePhone =
    DaPhone [
        ('1', " ")
        , ('2', "abc2")
        , ('3', "def3")
        , ('4', "ghi4")
        , ('5', "jkl5")
        , ('6', "mno6")
        , ('7', "pqrs7")
        , ('8', "tuv8")
        , ('9', "wxyz9")
        , ('*', "^*")
        , ('0', "_0")
        , ('#', ".,#")
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone@(DaPhone p) c
    | ord c <= 90 && ord c >= 65 = ('*', 1) : reverseTaps daPhone (chr (ord c + 32))
    | otherwise = case find (\(_, l) -> c `elem` l) p of
        Just (d, l) -> [(d, elemIndexOrZero c l + 1)]
        Nothing     -> []

elemIndexOrZero :: Char -> String -> Int
elemIndexOrZero c s = fromMaybe 0 (elemIndex c s)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> Char
mostPopularLetter s = go ' ' 0 ' ' 0 (sort s)
    where go currentC currentMax maxC max (x:xs)
            | null xs = maxC
            | currentC /= x && currentMax > max = go x 1 currentC currentMax xs
            | currentC /= x  = go x 1 maxC max xs
            | otherwise = go x (currentMax + 1) maxC max xs
