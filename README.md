# Haskell99Arrow

Problem 1: Find the last element of a list
```Haskell
p01_1 :: Arrow arrow => arrow [a]  a
p01_1 = arr reverse >>> arr head

--because of the law distributivity you can also write 
p01_1A :: Arrow arrow => arrow [a]  a
p01_1A = arr (reverse >>> head)

--the other way around using "<<<"
p01_2 :: Arrow (->) => (->) [a]  a -- or just p01_2 :: [a] -> a
p01_2 = head <<< reverse

--kleisli using maybe monad
p01_19A :: [a] -> Maybe a
p01_19A ls = runKleisli (kls >>> arr last) ls --here we actually need to "lift" "last" to a generic arrow so we can use it with the kleisli arrow to use the maybe monad
  where
    kls  = Kleisli (\list -> if null list then Nothing else Just list)
```
Problem 2: Find the last but one element of a list
```Haskell

```

Problem 3: 

```Haskell

```

Problem 4: 

```Haskell

```
Problem 5: 

```Haskell

```
Problem 6: Check if a given list is a palindrom 

```Haskell
--"pointfree"
p06_1 :: (Arrow arrow , Eq a )=> arrow  [a] Bool
p06_1 =  arr $ (  (flip div 2  <<<  length)  &&& id) >>> uncurry splitAt >>> second reverse >>> uncurry isPrefixOf

p06_2 :: (Arrow arrow , ArrowApply arrow, Eq a) => arrow [a] Bool
p06_2 = proc ls -> do
  len <- arr length -< ls
  firstHalf <- arr (take (div len 2)) -<< ls --here we need to use "-<<" istead of "-<" or otherwise "len" would not be in scope
  reversedLs <- arr reverse -< ls
  returnA -< isPrefixOf firstHalf reversedLs
```
Problem 7: 

```Haskell

```
Problem 8: 

```Haskell

```