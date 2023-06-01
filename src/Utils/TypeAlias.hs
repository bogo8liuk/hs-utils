{- Utilities to define "typed aliases", namely a type `a` that is identical to a type `t`,
but they are actually different at the eyes of the compiler. For example, the function `writeFile` has the following
type:

  writeFile :: FilePath -> String -> IO ()

where `FilePath` is defined as:

  type FilePath = String

`FilePath` is an alias to `String`, so we can accidentally flip the arguments and for the compiler everything will be ok!
This is very dangerous! But we can use the `Typing` type and make a definition of `FilePath` like the following:

  data FilePath_
  type FilePath = FilePath_ `Typing` String

Now, if we try to flip the arguments of `writeFile`, the compiler will raise an error.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Utils.TypeAlias
    ( Typing(..)
) where

import Control.Monad.Fix

{- Typing type `t` with `a` instead of aliasing `a` as `t`. -}
newtype Typing a t = Typing { unTyping :: t }

instance Eq t => Eq (a `Typing` t) where
    (==) (Typing x) (Typing y) = x == y

instance Ord t => Ord (a `Typing` t) where
    compare (Typing x) (Typing y) = compare x y

instance Semigroup t => Semigroup (a `Typing` t) where
    (<>) (Typing x) (Typing y) = Typing $ x <> y

instance Monoid t => Monoid (a `Typing` t) where
    mempty = Typing mempty

instance Show t => Show (a `Typing` t) where
    show (Typing x) = show x

deriving instance Read t => Read (a `Typing` t)

instance Functor (Typing a) where
    fmap f (Typing x) = Typing $ f x

instance Applicative (Typing a) where
    pure = Typing

    (<*>) (Typing f) (Typing x) = Typing $ f x

instance Monad (Typing a) where
    (>>=) (Typing x) op = op x

instance MonadFix (Typing a) where
    mfix f =
        let x = f (unTyping x) in x
