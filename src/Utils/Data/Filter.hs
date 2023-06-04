{- |
Module : Utils.Data.Filter
Description : Filter data type
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Interface for filter data type.
-}

module Utils.Data.Filter
    (
    -- * Membership interface
      Membership(..)
    -- * Filter data type
    , Filter(..)
) where

class Membership t e where
    isMember :: e -> t -> Bool
    isNotMember :: e -> t -> Bool

    isNotMember e t = not (e `isMember` t)

data Filter e
    = AllElements
    | NoElements
    | SomeElements [e]
    | ExceptElements [e]

instance Functor Filter where
    fmap _ AllElements = AllElements
    fmap _ NoElements = NoElements
    fmap f (SomeElements xs) = SomeElements $ fmap f xs
    fmap f (ExceptElements xs) = ExceptElements $ fmap f xs

instance Eq e => Membership (Filter e) e where
    isMember _ AllElements = True
    isMember _ NoElements = False
    isMember x (SomeElements xs) = x `elem` xs
    isMember x (ExceptElements xs) = x `notElem` xs
