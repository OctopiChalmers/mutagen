{-# OPTIONS_GHC -fplugin=Test.Mutagen.Tracer #-}
module RE.Match where

import RE.Types

----------------------------------------
-- | Regular expressions matcher


matches :: Eq a => RE a -> [a] -> Bool
matches re xs = match re (zip [0..] xs) null

match :: Eq a => RE a -> [(Int, a)] -> ([(Int, a)] -> Bool) -> Bool
match Nil        _      _ = False
match Eps        ics    k = k ics
match (Atom _)   []     _ = False
match (Atom a)   (x:xs) k = a == snd x && k xs
match (Star x)   ics    k = matchMany x ics k
match (Plus x y) ics    k = match x ics k || match y ics k
match (Seq x y)  ics    k = match x ics (\ics' -> match y ics' k)

matchMany :: Eq a => RE a -> [(Int, a)] -> ([(Int, a)] -> Bool) -> Bool
matchMany x ics k =
  k ics || match x ics (\ics' -> ics' `isAheadOf` ics && matchMany x ics' k)

isAheadOf :: [(Int, a)] -> [(Int, a)] -> Bool
isAheadOf []         []         = False
isAheadOf ((x, _):_) ((y, _):_) = x > y
isAheadOf _          _          = True

----------------------------------------
-- | Regular expressions optimizer

optimize :: Eq a => RE a -> RE a
optimize re = fixpoint reduce re

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a =
  let a' = f a
  in if a == a'
     then a
     else fixpoint f a'

reduce :: RE a -> RE a
reduce (Seq Eps re)       = reduce re
reduce (Seq re Eps)       = reduce re
reduce (Seq Nil _)        = Nil
reduce (Seq _ Nil)        = Nil
reduce (Seq x (Plus y z)) = Plus (reduce (Seq x y)) (reduce (Seq y z)) -- BUG!
reduce (Plus Nil re)      = reduce re
reduce (Plus re Nil)      = reduce re
reduce (Star Eps)         = Eps
reduce (Star (Star re))   = reduce (Star re)
reduce (Star re)          = Star (reduce re)
reduce (Plus x y)         = Plus (reduce x) (reduce y)
reduce (Seq x y)          = Seq (reduce x) (reduce y)
reduce re                 = re
