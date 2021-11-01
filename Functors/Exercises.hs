module Exercises where

import           GHC.Arr

data BoolAndSomethingElse a =
    False' a | True' a

instance Functor BoolAndSomethingElse where
    fmap f (False' x) = False' (f x)
    fmap f (True' x)  = True' (f x)

--

data BoolAndmaybeSomethingElse a =
    Falsish | Truish a

instance Functor BoolAndmaybeSomethingElse where
    fmap _ Falsish    = Falsish
    fmap f (Truish x) = Truish (f x)

---

data Sum b a =
    First a
    | Second b

instance Functor (Sum a) where
    fmap f (First a)  = First (f a)
    fmap f (Second b) = Second b

--

data Company a c b =
    DeepBlue a c
    | Something b

instance Functor (Company e e') where
    fmap f (Something b)  = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

--

data Quant a b =
    Finance
    | Desk a
    | Bloor b

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk x)  = Desk x
    fmap f (Bloor x) = Bloor (f x)

--

data List a =
    Nil
    | Cons a (List a)
    deriving Show

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons x l) = Cons (f x) (fmap f l)

--

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read fs)   = Read (fmap f fs)
