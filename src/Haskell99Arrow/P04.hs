{-# LANGUAGE Arrows #-}
module Haskell99Arrow.P04
    (
    ) where


import Control.Arrow

p04_1 :: [a] -> Int
p04_1 = proc ls ->
  if null ls
    then
      returnA -< 0
    else
      returnA -< 1 + p04_1 (tail ls)


--tailrecursive
p04_2 :: [a] -> Int
p04_2  ls = helper ls 0
  where

    helper :: [a] -> Int -> Int
    helper ls'  = proc i ->
      if null ls'
        then
          returnA -< i
        else
          returnA -< helper (tail ls')  (i + 1)
{-
--using rec
p04_3 :: [a] -> Int 
p04_3 ls =
-}

{-
--pointfree tailrecursive
p04_3 :: [a]  -> Int
-}
