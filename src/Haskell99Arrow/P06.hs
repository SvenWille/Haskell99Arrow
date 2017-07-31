{-# LANGUAGE Arrows #-}
module Haskell99Arrow.P06
    (
    ) where

import Control.Arrow
import Data.List (isPrefixOf)
import Control.Monad

--Problem 6: find out whether a list is a palindrom

--"pointfree"
p06_1 :: (Arrow arrow , Eq a )=> arrow  [a] Bool
p06_1 =  arr $ (  (flip div 2  <<<  length)  &&& id) >>> uncurry splitAt >>> second reverse >>> uncurry isPrefixOf


p06_2 :: (Arrow arrow , ArrowApply arrow, Eq a) => arrow [a] Bool
p06_2 = proc ls -> do
  len <- arr length -< ls
  firstHalf <- arr (take (div len 2)) -<< ls --here we need to use "-<<" istead of "-<" or otherwise "len" would not be in scope
  reversedLs <- arr reverse -< ls
  returnA -< isPrefixOf firstHalf reversedLs
