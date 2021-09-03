module Mood where

data Mood = Blah | Woot

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

instance Show Mood where
    show Blah = "Blah"
    show Woot = "Woot"
