{- |
Module : Utils.Data.Foldable
Description : Utilities on foldables and lists
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Set of utilities on @Foldable@ and @List@-like data types.
-}

module Utils.Data.Foldable
    ( insertAt
    , replaceAt
    , replaceAndGetAt
    , diff
    , diffDrop
    , diffDropTail
    , maybeSequence
    , allEq
    , occursAtLeastNTimes
    , head'
    , tail'
    , tail''
    , last'
    , heads
    , headsAndLast
    , elemAt
    -- * map functions
    , ixMap
    , lastSpecialMap
    , filterMap
    , splitMap
    , maybeMap
    , rightMap
    , thatmapFst
    , thismapFst
    --
    , isSublist
    , firstThat
    , lastThat
    , newNEFst
    , newNELast
    , maximumBy'
    , minimumBy'
    , theMost
    , greatest
    , lowest
    , theMost'
    , greatest'
    , lowest'
    , forAll
    , fromFstToLast
    , fromLastToFst
    , takeWhile'
    , indexing
    -- * functions on tuples
    , onFst
    , onSnd
) where

import Data.List(maximumBy, minimumBy, foldl', find)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Foldable(toList)

import Utils.Fancy
import Utils.Data.Knowledge
import Data.Semigroup (Last, getLast)

----------------------- Operations on lists and foldables -----------------------

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x [] = [x]
insertAt _ _ [] = []
insertAt n x l@(h : t)
    | n <= 0 = x : h : l
    | otherwise = h : insertAt (n - 1) x t

{- It replace an element at a given position (negative numbers are treated as zero). Complexity: O(pos), where pos is
the position where to replacing is performed, with 0 <= pos <= length l. -}
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n x l@(h : t)
    | n <= 0 = x : l
    | otherwise = h : replaceAt (n - 1) x t

{- Same of `replaceAt`, but it returns also the eventually removed element. NB: It is less efficient than `replaceAt`,
complexity: O(2 * pos). -}
replaceAndGetAt :: Int -> a -> [a] -> (Maybe a, [a])
replaceAndGetAt n x l =
    case splitAt n l of
        (l1, []) -> (Nothing, l1)
        (l1, h : t) -> (Just h, l1 ++ x : t)

diff :: Eq a => [a] -> [a] -> [a]
diff l1 l2 = filter (`elem` l2) l1

{- From the second list, it removes (from the head) the number of elements of the first list. -}
diffDrop :: [a] -> [b] -> [b]
diffDrop l = drop $ length l

{- Same as diffList but it removes starting from the tail. -}
diffDropTail :: [a] -> [b] -> [b]
diffDropTail l = reverse . drop (length l) . reverse

{- |
Specialization of @sequenceA@ with @Maybe@ value.
-}
maybeSequence :: Traversable t => t (Maybe a) -> Maybe (t a)
maybeSequence = sequenceA

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (h : t) = all (== h) t

occursAtLeastNTimes :: Eq a => a -> [a] -> Int -> Bool
occursAtLeastNTimes _ [] n = n <= 0
occursAtLeastNTimes x (h : t) n
    | n <= 0 = True
    | x == h = occursAtLeastNTimes x t $ n - 1
    | otherwise = occursAtLeastNTimes x t n

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : t) = Just t

{- Like tail, but if the list is empty, it returns the empty list. -}
tail'' :: [a] -> [a]
tail'' [] = []
tail'' (_ : t) = t

last' :: [a] -> Maybe a
last' l = head' $ reverse l

heads :: [a] -> Maybe [a]
heads [] = Nothing
heads l = Just $ heads' l
    where
        {- Using this function in order not to unwrap and wrap results. -}
        heads' [] = []    --This case should never be evaluated
        heads' [_] = []
        heads' (e : t) = e : heads' t

headsAndLast :: [a] -> Maybe ([a], a)
headsAndLast l = headsAndLast' l []
    where
        headsAndLast' [] _ = Nothing
        headsAndLast' [e] accum = Just (reverse accum, e)
        headsAndLast' (e : t) accum = headsAndLast' t $ e : accum

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
Alias for `find`.
-}
firstThat :: Foldable t => (a -> Bool) -> t a -> Maybe a
firstThat = find

{- |
Same of `firstThat`, but starting from the bottom.
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

rightMap :: Traversable t => (a -> Either c b) -> t a -> Either c (t b)
rightMap = traverse

thatmapFst :: (Foldable t, Functor t) => (a -> KnowledgeOneOf err b) -> t a -> KnowledgeOneOf err (t b)
thatmapFst f x =
    let y = fmap f x in
        case firstThat (not . isThat) $ toList y of
            Just None -> None
            Just (This err) -> This err
            _ -> That $ fmap (\(That e) -> e) y

thismapFst :: (Foldable t, Functor t) => (a -> KnowledgeOneOf b err) -> t a -> KnowledgeOneOf (t b) err
thismapFst f x =
    let y = fmap f x in
        case firstThat (not . isThis) $ toList y of
            Just None -> None
            Just (That err) -> That err
            _ -> This $ fmap (\(This e) -> e) y

newNEFst :: a -> [a] -> NonEmpty a
newNEFst = (:|)

newNELast :: [a] -> a -> NonEmpty a
newNELast [] e' = e' :| []
newNELast (e : t) e' = e :| t ++ [e']

{- Total version of maximumBy. -}
maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy' ord x
    | null x = Nothing
    | otherwise = Just $ maximumBy ord x

{- Total version of minimumBy. -}
minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
minimumBy' ord x
    | null x = Nothing
    | otherwise = Just $ minimumBy ord x

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

{- A more fancy version of foldl':

    forAll ["hello", "world", "42"] deleteKeyFrom strTable
-}
forAll :: Foldable t => t a -> (b -> a -> b) -> b -> b
forAll obj f start = foldl' f start obj

{- A more fancy version of foldl':

    fromFstToLast ["hello", "world", "42"] deleteKeyFrom strTable
-}
fromFstToLast :: Foldable t => t a -> (b -> a -> b) -> b -> b
fromFstToLast x f start = foldl' f start x

{- A more fancy version of foldr:

    fromLastToFst ["hello", "world", "42"] deleteKeyFrom strTable
-}
fromLastToFst :: Foldable t => t a -> (a -> b -> b) -> b -> b
fromLastToFst x f start = foldr f start x

{- Same of takeWhile, but it includes also the element that satisfy the predicate. -}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : t) =
    if f x
    then x : takeWhile' f t
    else [x]

indexing :: [a] -> [(Int, a)]
indexing = zip [0..]

----------------------- Operations on tuples -----------------------

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)
