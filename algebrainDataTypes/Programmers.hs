module Programmers where

data OperatingSystem =
    GnuPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show, Enum)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show, Enum)

data Programmer =
    Programmer { os   :: OperatingSystem
               , lang :: ProgLang}
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = enumFrom GnuPlusLinux

allLanguages :: [ProgLang]
allLanguages = enumFrom Haskell

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]
