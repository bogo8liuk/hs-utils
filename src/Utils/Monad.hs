{- |
Module : Utils.Monad
Description : Monad utilities
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Monad utilities.
-}

module Utils.Monad
    ( doNothing
    , pairA
    , notM
    , ixMapM
    , concatMapM
    , concatIxMapM
    , partitionM
    , fromFstToLastM
    , fromLastToFstM
    , (>>*)
    , tryFromFst
    , tryFromFst'
) where

import Data.List.NonEmpty(NonEmpty(..))
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Error.Class

{- |
Operation that does literally nothing.
-}
doNothing :: Applicative m => m ()
doNothing = pure ()

pairA :: Applicative m => m a -> m b -> m (a, b)
pairA = liftA2 (,)

notM :: Applicative m => m Bool -> m Bool
notM cond = not <$> cond

rawIxMapM :: Monad m => [a] -> Int -> (Int -> a -> m b) -> m [b]
rawIxMapM [] _ _ = return []
rawIxMapM (x : t) ix f = do
    x' <- f ix x
    t' <- rawIxMapM t (ix + 1) f
    return $ x' : t'

ixMapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
ixMapM f xs = rawIxMapM xs 0 f

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = do
    xss <- mapM f xs
    return $ concat xss

concatIxMapM :: Monad m => (Int -> a -> m [b]) -> [a] -> m [b]
concatIxMapM f xs = do
    xss <- rawIxMapM xs 0 f
    return $ concat xss

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f l = parts l ([], [])
    where
        parts [] (matching, notMatching) = return (reverse matching, reverse notMatching)
        parts (x : t) (matching, notMatching) = do
            isMatching <- f x
            if isMatching
            then parts t (x : matching, notMatching)
            else parts t (matching, x : notMatching)

{-|
More fancy version of foldM
-}
fromFstToLastM :: (Foldable t, Monad m) => t a -> (b -> a -> m b) -> b -> m b
fromFstToLastM t f x = foldM f x t

{- |
> fromLastToFstM [x1, x2, x3] f a

is semantically equal to

@
do
    a3 <- f x3 a
    a2 <- f x2 a3
    f x1 a2
@
-}
fromLastToFstM :: (Foldable t, Monad m) => t a -> (a -> b -> m b) -> b -> m b
fromLastToFstM t f x = foldr f' (pure x) t
    where
        f' e yM = do
            y <- yM
            f e y

{- |
A way to concatenate a list of operations with Monad bind. Useful to decrease the amount of code, if there's a long
chain of similar operations. For instance:

> f x = x >>* [g, h, g, m, h]

is semantically equal to

@
f x = do
    y <- g x
    z <- h y
    a <- g z
    b <- m a
    c <- h b
    return c
@

The syntax of >>* is also intuitive, because it executes >>= an arbitrary number of times (highlighted by * in >>*)
-}
(>>*) :: Monad m => a -> [a -> m a] -> m a
(>>*) = foldM (\y fM -> fM y)

{- |
It tries a list of actions, starting from the head, until one is successful; the remaining ones are not performed.
NB: it doesn't cancel the effects occurred when performing failing actions.
-}
tryFromFst :: (MonadPlus m, MonadError err m) => [m a] -> m a
tryFromFst [] = mzero
tryFromFst (op : ops) =
    op `catchError` \_ -> tryFromFst ops

{- |
Same of `tryFromFst`, but it doesn't require the actions to be a @MonadPlus@ instance.

NB: it doesn't cancel the effects occurred when performing failing actions.
-}
tryFromFst' :: MonadError err m => NonEmpty (m a) -> m a
tryFromFst' (op :| []) = op
tryFromFst' (op :| (op' : ops)) =
    op `catchError` \_ -> tryFromFst' (op' :| ops)
