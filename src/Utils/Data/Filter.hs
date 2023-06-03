module Utils.Data.Filter
    ( Filter(..)
    , isIncludedBy
    , isNotIncludedBy
) where

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

isIncludedBy :: Eq e => e -> Filter e -> Bool
isIncludedBy _ AllElements = True
isIncludedBy _ NoElements = False
isIncludedBy x (SomeElements xs) = x `elem` xs
isIncludedBy x (ExceptElements xs) = x `notElem` xs

isNotIncludedBy :: Eq e => e -> Filter e -> Bool
isNotIncludedBy x filt = not (x `isIncludedBy` filt)
