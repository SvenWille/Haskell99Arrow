{-# LANGUAGE Arrows #-}

module Haskell99Arrow.P03
    (
    ) where

import Control.Arrow


p03_1 :: [a] -> Int ->  a
p03_1 = (!!) >>> ( subtract 1 >>> )


p03_2 :: [a] -> Int -> [a]
p03_2  ls= proc i -> do
  tmp <- flip take ls -< i
  tmp2 <- flip drop ls -< i
  returnA -< (tmp ++ tmp2)
{-
--using the arrow syntax
p03_2 :: [a] -> Int -> Maybe a
p03_2 pos = proc ls ->
  if length < pos --using a check against length and then
    then
      returnA -< Nothing
    else
      rec
      -}
