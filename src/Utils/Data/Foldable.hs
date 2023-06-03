module Utils.Data.Foldable
    ( insertAt
    , replaceAt
    , replaceAndGetAt
    , diff
    , diffDrop
    , diffDropTail
    , getIfAll
    , allEq
    , occursAtLeastNTimes
    , head'
    , tail'
    , tail''
    , last'
    , heads
    , headsAndLast
    , elemAt
    , imap
    , lastmap
    , fltmap
    , splitmap
    , firstThat
    , lastThat
    , foldne
    , isSublist
    , maybemap
    , leftmapFst
    , rightmapFst
    , thatmapFst
    , thismapFst
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
    , onFst
    , onSnd
) where

import Data.List(maximumBy, minimumBy, foldl', find)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(isNothing)
import Data.Either(isRight, isLeft)
import Data.Foldable(toList)

import Utils.Fancy
import Utils.Data.Knowledge

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

{- Given a Foldable of Maybe, it returns a Maybe of Foldable of elements which were contained in the previous
Foldable: Nothing if at least one was Nothing, Just a Foldable otherwise (namely all elements were Just something). -}
getIfAll :: (Foldable t, Functor t) => t (Maybe a) -> Maybe (t a)
getIfAll = maybemap id

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (h : t) = all (== h) t

occursAtLeastNTimes :: Eq a => a -> [a] -> Int -> Bool
occursAtLeastNTimes _ [] n
    | n <= 0 = True
    | otherwise = False
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

{- Indexed version of map. -}
imap :: (Int -> a -> b) -> [a] -> [b]
imap f l = map <| uncurry f <| zip [0..] l

{- The same of map, but with a dedicated callback for the last element of the list. -}
lastmap :: (a -> b) -> (a -> b) -> [a] -> [b]
lastmap _ _ [] = []
lastmap _ flast [e] = [flast e]
lastmap f flast (e : t) = f e : lastmap f flast t

{- A filtered version of map: only just some elements get mapped. -}
fltmap :: (a -> Maybe b) -> [a] -> [b]
fltmap _ [] = []
fltmap f (e : t) =
    case f e of
        Nothing -> fltmap f t
        Just e' -> e' : fltmap f t

splitmap :: (a -> Maybe b) -> [a] -> ([b], [a])
{- NB: the elements are visited from the last to the first. -}
splitmap f = foldr f' ([], [])
    where
        f' x (incl, notIncl) =
            case f x of
                Nothing -> (incl, x : notIncl)
                Just x' -> (x' : incl, notIncl)

{- Alias for `find`. -}
firstThat :: Foldable t => (a -> Bool) -> t a -> Maybe a
firstThat = find

lastThat :: Foldable t => (a -> Bool) -> t a -> Maybe a
lastThat f l =
    fromLastToFst l findIt `startingFrom` Nothing
    where
        findIt _ found@(Just _) = found
        findIt x Nothing =
            if f x
            then Just x
            else Nothing

{- Total version of foldl1'. -}
foldne :: (a -> a -> a) -> [a] -> Maybe a
foldne _ [] = Nothing
foldne f l = Just $ foldl1 f l

{- It tests that all elements of the first list have at least one of the second list with which the equality
test returns true. -}
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist l l' = all (`elem` l') l

maybemap :: (Foldable t, Functor t) => (a -> Maybe b) -> t a -> Maybe (t b)
maybemap f x =
    let y = fmap f x in
        if any isNothing y
        then Nothing
        else Just $ fmap (\(Just e) -> e) y

leftmapFst :: (Foldable t, Functor t) => (a -> Either b err) -> t a -> Either (t b) err
leftmapFst f x =
    let y = fmap f x in
        case firstThat isRight $ toList y of
            Just (Right err) -> Right err
            _ -> Left $ fmap (\(Left e) -> e) y

rightmapFst :: (Foldable t, Functor t) => (a -> Either err b) -> t a -> Either err (t b)
rightmapFst f x =
    let y = fmap f x in
        case firstThat isLeft $ toList y of
            Just (Left err) -> Left err
            _ -> Right $ fmap (\(Right e) -> e) y

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
