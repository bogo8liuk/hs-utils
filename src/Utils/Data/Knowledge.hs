module Utils.Data.Knowledge
    ( KnowledgeOf(..)
    , isYes
    , isNo
    , isDK
    , KnowledgeOneOf(..)
    , isThis
    , isThat
    , isNone
) where

{- Like Maybe, but it offers one more case with the same type. -}
data KnowledgeOf a = Yes a | No a | Don'tKnow deriving (Show, Eq, Ord)

instance Functor KnowledgeOf where
    fmap f (Yes x) = Yes $ f x
    fmap f (No x) = No $ f x
    fmap _ Don'tKnow = Don'tKnow

isYes :: KnowledgeOf a -> Bool
isYes (Yes _) = True
isYes _ = False

isNo :: KnowledgeOf a -> Bool
isNo (No _) = True
isNo _ = False

isDK :: KnowledgeOf a -> Bool
isDK Don'tKnow = True
isDK _ = False

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
