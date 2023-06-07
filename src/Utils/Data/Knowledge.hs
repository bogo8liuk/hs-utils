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
    , isError
    , isOk
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
    (<>) (No x) (Yes y) = Yes (x <> y)
    (<>) (No x) (No y) = No (x <> y)

instance Functor KnowledgeOf where
    fmap f (Yes x) = Yes $ f x
    {- |
    Here, there's the main particularity of this type: the fact that also `No` constructor is mapped. Note that
    if `KnowledgeOf` was an alias of `KnowledgeOneOf`, it wouldn't be possible to map `No` constructor.
    -}
    fmap f (No x) = No $ f x
    fmap _ Don'tKnow = Don'tKnow

instance Applicative KnowledgeOf where
    pure = Yes

    {- |
    Look at @Functor@ instance to see the note about `No` costructor mapping.
    -}
    (<*>) Don'tKnow _ = Don'tKnow
    (<*>) _ Don'tKnow = Don'tKnow
    (<*>) (Yes f) (Yes x) = Yes $ f x
    (<*>) (Yes f) (No x) = No $ f x
    (<*>) (No f) (Yes x) = No $ f x
    (<*>) (No f) (No x) = No $ f x

instance Foldable KnowledgeOf where
    foldMap _ Don'tKnow = mempty
    foldMap f (Yes x) = f x
    {- |
    Here, the same thing happing also in the @Functor@ instance.
    -}
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

{- |
Like a @Maybe@ of @Either@.
-}
data KnowledgeOneOf e a = Error e | Ok a | None deriving (Show, Eq, Ord)

instance Semigroup (KnowledgeOneOf e a) where
    (<>) None k = k
    (<>) k None = k
    (<>) (Error _) k = k
    (<>) k _ = k

instance Functor (KnowledgeOneOf a) where
    fmap _ None = None
    fmap _ (Error e) = Error e
    fmap f (Ok x) = Ok $ f x

instance Applicative (KnowledgeOneOf a) where
    pure = Ok

    (<*>) None _ = None
    (<*>) (Error e) _ = Error e
    (<*>) (Ok f) x = fmap f x

instance Foldable (KnowledgeOneOf a) where
    foldMap _ None = mempty
    foldMap _ (Error _) = mempty
    foldMap f (Ok x) = f x

instance Traversable (KnowledgeOneOf a) where
    traverse _ None = pure None
    traverse _ (Error e) = pure $ Error e
    traverse f (Ok x) = Ok <$> f x

isError :: KnowledgeOneOf a b -> Bool
isError (Error _) = True
isError _ = False

isOk :: KnowledgeOneOf a b -> Bool
isOk (Ok _) = True
isOk _ = False

isNone :: KnowledgeOneOf a b -> Bool
isNone None = True
isNone _ = False
