{- |
Module : Utils.Data.Root
Description : Root type class
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Root type class for those types which may have the notion of a \"root element\".
Usually, recursive data structures can be `Root`.
-}

module Utils.Data.Root
    ( Root(..)
) where

import Data.Tree (Tree (..))
import Utils.Data.Foldable (head')

class Root r where
    {- |
    Getting the root element, if possible.
    -}
    root :: r a -> Maybe a
    {- |
    The same of `fmap` for Functors, but mapping just the root element and
    restricting the type of the update function.
    -}
    rootMap :: (a -> a) -> r a -> r a

instance Root [] where
    root = head'

    rootMap _ [] = []
    rootMap f (x : xs) = f x : xs

instance Root Tree where
    root = Just . rootLabel

    rootMap f node =
        node { rootLabel = f $ rootLabel node }
