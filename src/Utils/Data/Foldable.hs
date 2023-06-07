{- |
Module : Utils.Data.Foldable
Description : Utilities on foldables and lists
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Set of utilities on @Foldable@ and @List@-like data types.
-}

module Utils.Data.Foldable
    (
    -- * Functions on lists
      elemAt
    , insertAt
    , replaceAt
    , replaceAndGetAt
    , diff
    , diffDrop
    , diffDropTail
    , allEq
    , occurs
    , head'
    , tail'
    , tailEmptyMin
    , last'
    , discardLast
    , splitLast
    , isSublist
    , theMost
    , greatest
    , lowest
    , takeWhile'
    , indexing
    -- ** Functions on non-empty lists
    , newNonEmpty
    , newNonEmpty'
    , theMost'
    , greatest'
    , lowest'
    -- * Mapping functions
    , ixMap
    , lastSpecialMap
    , filterMap
    , splitMap
    , maybeMap
    , eitherMap
    , knowMap
    , knowOneMap
    -- * Functions on foldables
    , firstThat
    , lastThat
    , maximumBy'
    , minimumBy'
    , fromFstToLast
    , fromLastToFst
    -- * functions on tuples
    , onFst
    , onSnd
    -- * Other utilities
    , maybeSequence
) where

import Data.List (maximumBy, minimumBy, foldl', find)
import Data.List.NonEmpty (NonEmpty(..))
import Utils.Fancy
import Utils.Data.Knowledge
import Data.Semigroup (Last, getLast)

{- |
It inserts an element in a list at a given index. If the index is out of the list bounds, it does not insert the
element.
-}
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x [] = [x]
insertAt _ _ [] = []
insertAt n x l@(h : t)
    | n == 0 = x : h : l
    | otherwise = h : insertAt (n - 1) x t

{- |
It replaces an element at a given position (negative numbers are treated as zero).

Complexity: /O(pos)/, where /pos/ is the position where to replacing is performed, with /0 <= pos <= length l/.
-}
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n x l@(h : t)
    | n <= 0 = x : l
    | otherwise = h : replaceAt (n - 1) x t

{- |
Same of `replaceAt`, but it returns also the eventually removed element.

__NB__: It is less efficient than `replaceAt`.

Complexity: /O(2 * pos)/.
-}
replaceAndGetAt :: Int -> a -> [a] -> (Maybe a, [a])
replaceAndGetAt n x l =
    case splitAt n l of
        (l1, []) -> (Nothing, l1)
        (l1, h : t) -> (Just h, l1 ++ x : t)

{- |
It removes all the elements of the first list which stand also in the second list.

> "Hello!" `diff` "!o" == "Hell"
-}
diff :: Eq a => [a] -> [a] -> [a]
diff l1 l2 = filter (`elem` l2) l1

{- |
From the first list, it removes (from the head) the number of elements of the second list.
-}
diffDrop :: [a] -> [b] -> [a]
diffDrop [] _ = []
diffDrop l1 [] = l1
diffDrop (_ : t1) (_ : t2) = diffDrop t1 t2

{- |
Same as `diffDrop` but it removes starting from the tail.
-}
diffDropTail :: [a] -> [b] -> [a]
diffDropTail l1 l2 = fst $ foldr dropThenAccum ([], l2) l1
    where
        -- | While the second component has elements, don't accumulate. Whenever it becomes empty, start accumulating.
        dropThenAccum x (accum, []) = (x : accum, [])
        dropThenAccum _ (accum, _ : t) = (accum, t)

{- |
Specialization of @sequenceA@ with @Maybe@ value.
-}
maybeSequence :: Traversable t => t (Maybe a) -> Maybe (t a)
maybeSequence = sequenceA

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (h : t) = all (== h) t

{- |
It tells if an element occurs at least a given number of times in a list.
-}
occurs :: Eq a => Int -> a -> [a] -> Bool
occurs n _ [] = n <= 0
occurs n x (h : t)
    | n <= 0 = True
    | x == h = occurs (n - 1) x t
    | otherwise = occurs n x t

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : t) = Just t

{- |
Like @tail@, but if the list is empty, it returns the empty list.
-}
tailEmptyMin :: [a] -> [a]
tailEmptyMin [] = []
tailEmptyMin (_ : t) = t

last' :: [a] -> Maybe a
last' l = head' $ reverse l

{- |
If the list is not empty, it returns the list without the last element, else it returns @Nothing@.
-}
discardLast :: [a] -> Maybe [a]
discardLast [] = Nothing
discardLast l = Just $ heads l
    where
        heads [] = []    -- ^ This case should never be evaluated
        heads [_] = []
        heads (e : t) = e : heads t

{- |
Same of `discardLast`, but it returns the last element separately, instead of discarding it.
-}
splitLast :: [a] -> Maybe ([a], a)
splitLast l = headsAndLast' l []
    where
        headsAndLast' [] _ = Nothing
        headsAndLast' [e] accum = Just (reverse accum, e)
        headsAndLast' (e : t) accum = headsAndLast' t $ e : accum

{- |
It takes the n-th element of the list. If the index is out of the bounds of the list, it returns @Nothing@.
-}
elemAt :: Int -> [a] -> Maybe a
elemAt _ [] = Nothing
elemAt 0 (h : _) = Just h
elemAt n (_ : t) = elemAt (n - 1) t

{- |
Indexing version of @map@.
-}
ixMap :: (Int -> a -> b) -> [a] -> [b]
ixMap f l = ixMap' l 0
    where
        ixMap' [] _ = []
        ixMap' (h : t) n = f n h : ixMap' t (n + 1)

{- |
The same of @map@, but with a dedicated callback for the last element of the list. Using @Last@ data type in order to
type the callback for the last element since it has the same type of function applied to the other elements of the list.
-}
lastSpecialMap :: (a -> b) -> Last (a -> b) -> [a] -> [b]
lastSpecialMap _ _ [] = []
lastSpecialMap _ flast [e] = [getLast flast e]
lastSpecialMap f flast (e : t) = f e : lastSpecialMap f flast t

{- |
A filtering version of @map@: only elements which map to @Just@ values are returned.

NB: the elements are visited from the last to the first.
-}
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr tryToAdd []
{- An alternative implementation could be using the first component of the result of `splitMap`, but doing like that
would allocate useless thunks (???) -}
    where
        tryToAdd x accum =
            case f x of
                Nothing -> accum
                Just y -> y : accum

{- |
Splitting version of @map@: elements which don't map to @Just@ values are returned as they are.
NB: the elements are visited from the last to the first.
-}
splitMap :: (a -> Maybe b) -> [a] -> ([b], [a])
splitMap f = foldr f' ([], [])
    where
        f' x (incl, notIncl) =
            case f x of
                Nothing -> (incl, x : notIncl)
                Just y -> (y : incl, notIncl)

{- |
Alias for @find@.
-}
firstThat :: Foldable t => (a -> Bool) -> t a -> Maybe a
firstThat = find

{- |
It takes the last element which respects the predicate. The implementation does not test all elements.
-}
lastThat :: Foldable t => (a -> Bool) -> t a -> Maybe a
lastThat f l =
    fromLastToFst l findIt `startingFrom` Nothing
    where
        findIt _ found@(Just _) = found
        findIt x Nothing =
            if f x
            then Just x
            else Nothing

{- |
It tests that all elements of the first list have at least one equal element in the second list.
-}
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist l l' = all (`elem` l') l

{- |
Specialization of @traverse@ with @Maybe@ values.
-}
maybeMap :: Traversable t => (a -> Maybe b) -> t a -> Maybe (t b)
maybeMap = traverse

{- |
Specialization of @traverse@ with @Either@ values.
-}
eitherMap :: Traversable t => (a -> Either c b) -> t a -> Either c (t b)
eitherMap = traverse

{- |
Specialization of @traverse@ with @KnowledgeOf@ values.
-}
knowMap :: Traversable t => (a -> KnowledgeOf b) -> t a -> KnowledgeOf (t b)
knowMap = traverse

{- |
Specialization of @traverse@ with @KnowledgeOneOf@ values.
-}
knowOneMap :: Traversable t => (a -> KnowledgeOneOf c b) -> t a -> KnowledgeOneOf c (t b)
knowOneMap = traverse

{- |
Alias for @(:|)@.
-}
newNonEmpty :: a -> [a] -> NonEmpty a
newNonEmpty = (:|)
{-# INLINE newNonEmpty #-}

newNonEmpty' :: [a] -> a -> NonEmpty a
newNonEmpty' [] e' = e' :| []
newNonEmpty' (e : t) e' = e :| t ++ [e']

{- |
Total version of @maximumBy@.
-}
maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy' ord x
    | null x = Nothing
    | otherwise = Just $ maximumBy ord x

{- |
Total version of minimumBy.
-}
minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
minimumBy' ord x
    | null x = Nothing
    | otherwise = Just $ minimumBy ord x

{- |
It finds the element of the list which, applied to the predicate, one by one with all other elements of the list,
returns @True@. It is particularly useful for predicates which respect transitivity.

> theMost (>) [1, 42, 7, 3] == 42
-}
theMost :: (a -> a -> Bool) -> [a] -> Maybe a
theMost _ [] = Nothing
theMost p (h : t) = Just $ theMost' p (h :| t)

greatest :: Ord a => [a] -> Maybe a
greatest = theMost (>)

lowest :: Ord a => [a] -> Maybe a
lowest = theMost (<)

theMost' :: (a -> a -> Bool) -> NonEmpty a -> a
theMost' p (h :| l) = foldl' keep h l
    where
        keep x y
            | y `p` x = y
            | otherwise = x

greatest' :: Ord a => NonEmpty a -> a
greatest' = theMost' (>)

lowest' :: Ord a => NonEmpty a -> a
lowest' = theMost' (<)

{- |
A more fancy version of @foldl\'@:

> fromFstToLast ["hello", "world", "42"] deleteKeyFrom strTable
-}
fromFstToLast :: Foldable t => t a -> (b -> a -> b) -> b -> b
fromFstToLast x f start = foldl' f start x

{- |
A more fancy version of @foldr@:

> fromLastToFst ["hello", "world", "42"] deleteKeyFrom strTable
-}
fromLastToFst :: Foldable t => t a -> (a -> b -> b) -> b -> b
fromLastToFst x f start = foldr f start x

{- |
Same of @takeWhile@, but it includes also the element that satisfy the predicate.
-}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : t) =
    if f x
    then x : takeWhile' f t
    else [x]

indexing :: [a] -> [(Int, a)]
indexing = zip [0..]

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)
