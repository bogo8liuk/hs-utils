# hs-utils

This is a small library with the aim of gathering some utility functions which I usually use when programming in
Haskell. The goal is to create more idiomatic Haskell code, by simplifying some operations and renaming some functions
in a more fancy way.

### Examples
- Example with `fromFstToLast` reduce function and `try'`, `else'` and `startingFrom` functions:

```Haskell
defaultValue :: Integer
defaultValue = 0

fetch :: String -> [(String, Integer)] -> Integer
fetch key pairs =
    try'
        (fromFstToLast pairs findValue `startingFrom` Nothing)
    `else'`
        defaultValue
    where
        findValue found @ (Just _) _ = found
        findValue Nothing (k, v) =
            if k == key
            then Just v
            else Nothing
```

- Example with `>>*` monadic operator, `ixMapM` mapping function and `doNothing` function:
```Haskell
plusM :: Int -> Int -> IO Int
plusM x x' = pure (x + x')

minusM :: Int -> Int -> IO Int
minusM x x' = pure (x - x')

multM :: Int -> Int -> IO Int
multM x x'= pure (x * x')

findAnswer :: [Int] -> IO ()
findAnswer xs = do
    xs' <- xs >>* [mapM (`minusM` 3), ixMapM plusM, mapM (multM 2)]
    if sum xs' == 42
    then putStrLn "You found the answer!"
    else doNothing
```

### TODO
- [ ] Testing correctness of each function
