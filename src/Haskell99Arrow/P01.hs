{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell99Arrow.P01
    (p01_1,p01_2
    ) where

--Problem 01: find the last element of a list

--this problem is used introduces a big part of the available arrow operators/functions (even though some solutions are rather constructed and not so helpful).
--the essential functions (&&&) (***) first and second are introduced in the other problems (since they will be used fairly regularly)
import Control.Arrow
import Data.Bool.HT
import Control.Monad
import Data.Maybe (fromJust)
import Control.Monad.Fix
import Control.Monad.State.Lazy

--simple, unsafe solution
--arr lifts (like liftM) a funktion to an arrow, since functions are
--an instance of we don't need to lift functions ,unless we want to use it with other arrows
--if for example one wants to combine a Kleisli arrow with function arrow one has to lift function arrows
p01_1 :: Arrow arrow => arrow [a]  a
p01_1 = arr reverse >>> arr head

--because of distributivity this is the same as above
p01_1A :: Arrow arrow => arrow [a]  a
p01_1A = arr (reverse >>> head)

--the other way around using "<<<"
p01_2 :: Arrow (->) => (->) [a] a -- or just p01_2 :: [a] -> a
p01_2 = head <<< reverse


--using arrow syntax
p01_3 :: [a] -> a
p01_3  =  proc ls -> do
  tmp <- reverse -< ls -- or in one line wiht "head <<< reverse -< ls"
  head -< tmp

--safe version
p01_4 :: [a] -> Maybe a
p01_4 = proc ls ->
  if null ls
    then
      returnA -< Nothing
    else do
      tmp <- reverse -< ls
      Just <<< head -<  tmp

--using case syntax
p01_5 :: [a] -> Maybe a
p01_5 = proc ls ->
  case ls of
    [] -> returnA -< Nothing
    _ -> Just <<< head <<< reverse -< ls


--another safe, pointfree-like version
p01_6 :: Arrow arrow => arrow [a]  (Maybe a)
p01_6 = arr $ join ( null >>> flip if' Nothing >>> ((last >>> Just) >>> ))


--simplified less pointfree
p01_7 :: Arrow (->) => (->)  [b] (Maybe b)
p01_7 ls =
  if null ls
    then Nothing
    else curry app  (reverse >>> head >>> Just) ls


--constructed example to show the usage of arrowChoice
p01_8 :: [a] -> Maybe a
p01_8  =  (\ls -> if null ls then Left Nothing else Right ls ) >>>  id ||| (reverse >>> head >>> Just)


p01_8A ::Arrow (->) => (->) [a]  (Maybe a)
p01_8A = arr (\ls -> if null ls then Left Nothing else Right ls) >>> (arr id) ||| (arr (\ls -> if length ls == 1 then Just $ head ls else p01_8A $ tail ls))


p01_8B ::ArrowChoice arrow => arrow [a]  (Maybe a)
p01_8B = arr (\ls -> if null ls then Left Nothing else Right ls) >>> (arr id) ||| (arr (\ls -> if length ls == 1 then Just $ head ls else p01_8A $ tail ls))

--another constructed example to show the usage of "+++"
p01_9 :: [a] -> Either String a
p01_9 = (\ls -> if null ls then Left "replace me" else Right ls) >>> const "no such element" +++ (reverse >>> head)

--another constructed example to show the usage of right (left works the same way)
p01_10 :: [a] -> Either String a
p01_10 = (\ls -> if null ls then Left "no such element" else Right ls) >>> right (reverse >>> head)

--demonstration of precomposition (for functions , this works like ">>>" or "<<<" respectively)
p01_11 :: [a] -> a
p01_11 = reverse ^>> head

--the other way around and with "head" liftet to an arrow
p01_12 :: Arrow arrow => arrow [a] a
p01_12 = arr head <<^ reverse

--example showing "app"
p01_13 :: Arrow arrow => arrow [a] a
p01_13 = arr (\ x ->  app (arr last , x))

--recursive
p01_14 :: [a] -> Maybe a
p01_14 = proc ls ->
  case ls of
    [] -> returnA -< Nothing
    [x] -> returnA -< Just x
    (x:xs) -> returnA -< p01_14 xs

  --using  "-<<" (this operator works the same way as -< but is needed here because it is bound bu "proc")
p01_15 :: [a] -> Maybe a
p01_15 = proc ls -> do
  let lst ls' =   if length  ls' > 1 then   lst (tail ls')else  Just (head ls')
  lst -<< ls

--example showing the usage of loop
p01_16 :: [a] -> Maybe a
p01_16 = loop (\(output,feedback)-> (feedback output , \ls -> case ls of ; [] -> Nothing ; [x] -> Just x; (x:xs) -> feedback xs) )

--using the "rec"-keyword
p01_17 :: [a] -> Maybe a
p01_17 = proc ls -> do
  rec lst <- returnA -< \ls' -> case ls' of; [] -> Nothing; [x] -> Just x; (x:xs) -> lst xs
  lst -<< ls --or returnA -< lst ls

--using leftApp to use a choosen default value in case the list is empty (also a rather constructed example just to show how leftApp works).
--There is no rightApp (only right, as mentioned above)
--left app works basically like left (for the function instance)
p01_18 :: [a] -> a -> Either a a
p01_18 ls deflt= leftApp (const deflt) $ if null ls then Left "replace me" else Right $ last ls

--using kleisli arrows (non sense introdutiory example)
p01_19 :: [a] -> a -> a
p01_19 =  runKleisli $ Kleisli (\ls deflt -> if null ls then deflt else last ls )

--kleisli using maybe monad
p01_19A :: [a] -> Maybe a
p01_19A ls = runKleisli (kls >>> arr last) ls --here we actually need to "lift" "last" to a generic arrow so we can use it with the kleisli arrow to use the maybe monad
  where
    kls  = Kleisli (\list -> if null list then Nothing else Just list)

--another version for better understanding
p01_19B :: [a] -> Maybe a
p01_19B ls = runKleisli (Kleisli kls >>>  Kleisli (Just . last)) ls
  where
    kls  =  (\list -> if null list then Nothing else Just list)




--using the kleisli version of "loop"

test :: [a] -> Maybe a
test ls = fst $ runState (mfix helper) ls
  where
    helper recc = do
      val <- get
      case val of
        [] -> return Nothing
        [x] -> return (Just x)
        (x:xs) -> do
          put xs
          return recc

p01_20 :: [a] -> Maybe a
p01_20 = loop
  where
    exampleKleisli  = Kleisli (\(,recc))


--using banana brackets (usually banana brackets are used as combinators for hihger orderfunctions which take more than two functions)
p01_21 :: [a] -> Maybe a
p01_21 = proc ls -> (| (\fn -> fn) ( (\list -> if null list then Just $ last list else Nothing  )>>> returnA -< ls )  |)

--using arrow monad, simple version , to show how the ArrowMonad type COULD be used (for better understanding that more or less strange type)
p01_22 :: Arrow arrow => arrow [a] a
p01_22 = arr $  (\(ArrowMonad m) ls -> m () ls) am
  where
    am = ArrowMonad (const last >>> id)




{-
p01_23 ::  [a] -> a
p01_23  ls =   (\(ArrowMonad m) -> m ()  ) (am >>= )
  where
s    am = ArrowMonad (arr $ const ls )
    -}
{-
--ArrowMonad with Maybe
p01_24 :: [a] -> Maybe a
p01_24 =
  where
-}

{-
--constructed example using arrowplus
p01_24 :: [a] -> Maybe a
p01_24 =
-}

--non sense mutual recursive version
