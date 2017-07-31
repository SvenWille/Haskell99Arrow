{-# LANGUAGE Arrows #-}
module Haskell99Arrow.P33
    (p33_1, p33_2, p33_3
    ) where

import Control.Arrow

p33_1 :: Int -> Int -> Bool
p33_1 = gcd >>> ( >>>  (== 1))

--redundant nonsensical version
p33_2 :: Int -> Int -> Bool
p33_2 = ((== 1) <<< ) <<< curry app <<< curry app gcd


--arrow syntax
p33_3 :: Int  -> Int  ->  Bool
p33_3 x  = proc y ->  returnA -< gcd x y == 1

--arrow syntax
p33_4 :: Int  -> Int  ->  Bool
p33_4  = curry (proc (x,y) ->  returnA -< gcd x y == 1)
