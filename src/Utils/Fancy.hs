{- |
Module : Utils.Fancy
Description : Sugar functions
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Definitions of utilities with the aim of making the code just more fancy and idiomatic.
-}

module Utils.Fancy
    ( (|>)
    , (<|)
    , if'
    , then'
    , else'
    , try'
    , startingFrom
    , accumulatingIn
) where

import Data.Maybe(fromMaybe)

{- |
This is the same of @($)@ operator, but it is left-associative instead of being right-associative like @($)@.
Let's make an example to understand its usefullness, we have:

@
f x y z = x + y + z
...
f <| 4 + 5 <| 2 + 3 - 5 <| 1
@

This is useful to nest intermediate operations to pass to a function without parenthesis.
-}
infixl 1 <|
(<|) :: (a -> b) -> a -> b
(<|) f = f

{- |
Operator useful for reverse application:

> [1, 2, 3] |> map (* 2) |> intersperse 15 |> sum

is semantically equal to

> sum . intersperse 15 . map (* 2) $ [1, 2, 3]
-}
infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

{- |
The if-then-else construct without the if-then-else construct! Usage example:

@
if' (x > y)
`then'` "Hello World!"
`else'` "World! Hello"
@

The only limitation is that the if-condition has to be inside parenthesis.
-}
if' :: Bool -> Bool
if' = id

then' :: Bool -> a -> (a -> a)
then' True x = const x
then' False _ = id

else' :: (a -> a) -> a -> a
else' = ($)

{- |
More fancy version of @fromMaybe@:

@
try'
    doSomethingOn x
`else'`
    takeAlternative y
@
-}
try' :: Maybe a -> a -> a
try' = flip fromMaybe

{- |
More fancy versions of application. For example:

> fromFstToLast ["hello", "world", "!!!"] fetchVowels `accumulatingIn` (x, [])
-}
infixl 0 `startingFrom`
startingFrom :: (a -> b) -> a -> b
startingFrom = ($)

infixl 0 `accumulatingIn`
accumulatingIn :: (a -> b) -> a -> b
accumulatingIn = ($)
