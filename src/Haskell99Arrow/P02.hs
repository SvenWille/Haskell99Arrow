{-# LANGUAGE Arrows #-}
module Haskell99Arrow.P02
    (p02_1,
    ) where

--Problem 2: find the last but one element of a list

import Control.Arrow


p02_1 :: [a] -> a
p02_1 = init >>> reverse >>> head

--other way around

p02_2 :: Arrow arrow => arrow [a] a
p02_2 = arr (head <<< reverse <<< init)


p02_3 :: [a] -> Maybe a
p02_3 = proc ls ->
    if length ls < 2
      then returnA -< Nothing
      else init >>> last >>> Just -< ls

{-
--using loop
p02_4 :: [a] -> Maybe a
p02_4 = loop


--pointfree
p02_5 :: [a] -> Maybe a
p02_5 =
-}
