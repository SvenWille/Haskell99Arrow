{-# LANGUAGE Arrows #-}
module Haskell99Arrow.P05
    (
    ) where

import Control.Arrow

f :: a -> a
f = proc x -> (| undefined |)

{-
--using loop
p05_1 :: Arrow arrow => arrow [a] [a]
p05_1 =
-}
