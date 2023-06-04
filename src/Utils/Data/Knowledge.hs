{- |
Module : Utils.Data.Knowledge
Description : Compositions of Maybe and Either data types
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Compositions of @Maybe@ and @Either@ data types. You should use directly @Maybe@ and @Either@ since they are more
stable and optimized. You can use this if you need to have newtypes with different constructors.
-}

module Utils.Data.Knowledge
    ( KnowledgeOf(..)
    , isYes
    , isNo
    , isDon'tKnow
    , KnowledgeOneOf(..)
    , isThis
    , isThat
    , isNone
) where

{- |
Like @Maybe@, but it has one more variant.
-}
data KnowledgeOf a = Yes a | No a | Don'tKnow deriving (Show, Eq, Ord)

instance Semigroup a => Semigroup (KnowledgeOf a) where
    (<>) Don'tKnow x = x
    (<>) x Don'tKnow = x
    (<>) (Yes x) (Yes y) = Yes (x <> y)
    (<>) (Yes x) (No y) = Yes (x <> y)
    (<>) (No x) (Yes y) = No (x <> y)
    (<>) (No x) (No y) = No (x <> y)

instance Functor KnowledgeOf where
    fmap f (Yes x) = Yes $ f x
    fmap f (No x) = No $ f x
    fmap _ Don'tKnow = Don'tKnow

instance Applicative KnowledgeOf where
    pure = Yes

    (<*>) Don'tKnow _ = Don'tKnow
    (<*>) _ Don'tKnow = Don'tKnow
    (<*>) (Yes f) (Yes x) = Yes $ f x
    (<*>) (Yes f) (No x) = Yes $ f x
    (<*>) (No f) (Yes x) = No $ f x
    (<*>) (No f) (No x) = No $ f x

instance Foldable KnowledgeOf where
    foldMap _ Don'tKnow = mempty
    foldMap f (Yes x) = f x
    foldMap f (No x) = f x

    null Don'tKnow = True
    null _ = False

instance Traversable KnowledgeOf where
    traverse _ Don'tKnow = pure Don'tKnow
    traverse f (Yes x) = Yes <$> f x
    traverse f (No x) = No <$> f x

isYes :: KnowledgeOf a -> Bool
isYes (Yes _) = True
isYes _ = False

isNo :: KnowledgeOf a -> Bool
isNo (No _) = True
isNo _ = False

isDon'tKnow :: KnowledgeOf a -> Bool
isDon'tKnow Don'tKnow = True
isDon'tKnow _ = False

{- Like Maybe Either, but using a single type. -}
data KnowledgeOneOf a b = This a | That b | None deriving (Show, Eq, Ord)

isThis :: KnowledgeOneOf a b -> Bool
isThis (This _) = True
isThis _ = False

isThat :: KnowledgeOneOf a b -> Bool
isThat (That _) = True
isThat _ = False

isNone :: KnowledgeOneOf a b -> Bool
isNone None = True
isNone _ = False
