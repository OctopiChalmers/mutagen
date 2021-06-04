{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Mutagen.Fragment where

import Data.Typeable

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

-- import Test.Mutagen.Mutation (Pos)

-- For providing some default Fragmentable instances
import Data.Word

----------------------------------------
-- Test case fragments as existential typeable values

type IsFragment a = (Typeable a, Ord a, Show a)

data Fragment = forall a. IsFragment a => Fragment a

instance Eq Fragment where
  Fragment f1 == Fragment f2 =
    case cast f2 of
      Just f2' -> f1 == f2'
      Nothing  -> False

instance Ord Fragment where
  compare (Fragment f1) (Fragment f2) =
    case cast f2 of
      Just f2' -> compare f1 f2'
      Nothing  -> LT
      -- This ^ shouldn't be needed because the fragments should be of the same
      -- type at this point. Nonetheless, I hope it works!!

instance Show Fragment where
  show (Fragment a) = show a

----------------------------------------
-- Storing fragments based on their type

newtype FragmentStore = FragmentStore (Map TypeRep (Set Fragment))
  deriving Show

fragmentStoreSize :: FragmentStore -> [(TypeRep, Int)]
fragmentStoreSize (FragmentStore fs) = [ (tr, Set.size frags) | (tr, frags) <- Map.toList fs ]

instance Semigroup FragmentStore where
  FragmentStore fs1 <> FragmentStore fs2 =
    FragmentStore (Map.unionWith Set.union fs1 fs2)

instance Monoid FragmentStore where
  mempty = emptyFragmentStore

emptyFragmentStore :: FragmentStore
emptyFragmentStore = FragmentStore mempty

insertFragment :: TypeRep -> Fragment -> FragmentStore -> FragmentStore
insertFragment tr fr (FragmentStore fs) =
  FragmentStore (Map.insertWith Set.union tr (Set.singleton fr) fs)

storeFragments :: Fragmentable a => a -> FragmentStore -> FragmentStore
storeFragments a fs = fs <> fs'
  where
    fs' = foldr (uncurry insertFragment) emptyFragmentStore fts
    fts = Set.map (\(Fragment x) -> (typeOf x, Fragment x)) (fragmentize a)

----------------------------------------
-- Fragmentizing values up to a certain depth

class IsFragment a => Fragmentable a where
  fragmentize :: a -> Set Fragment
  fragmentize = singleton

-- Helpers

singleton :: Fragmentable a => a -> Set Fragment
singleton = Set.singleton . Fragment

----------------------------------------
-- | Fragmentable instances
----------------------------------------

instance Fragmentable Int
instance Fragmentable Double
instance Fragmentable Float
instance Fragmentable Word8
instance Fragmentable Word16
instance Fragmentable Word32
instance Fragmentable Word64
instance Fragmentable Char
instance Fragmentable Bool

instance Fragmentable a => Fragmentable (Maybe a) where
  fragmentize Nothing  = singleton @(Maybe a) Nothing
  fragmentize (Just x) = singleton @(Maybe a) (Just x) <> fragmentize x

instance (Fragmentable a, Fragmentable b) => Fragmentable (Either a b) where
  fragmentize (Left x)  = singleton @(Either a b) (Left x)  <> fragmentize x
  fragmentize (Right x) = singleton @(Either a b) (Right x) <> fragmentize x

instance Fragmentable a => Fragmentable [a] where
  fragmentize []     = singleton @[a] []
  fragmentize (x:xs) = singleton @[a] (x:xs) <> fragmentize x <> fragmentize xs

-- Tuple instances

instance (Fragmentable a, Fragmentable b) => Fragmentable (a, b) where
  fragmentize (a, b) = singleton @(a, b) (a, b) <> fragmentize a <> fragmentize b

instance (Fragmentable a, Fragmentable b, Fragmentable c) => Fragmentable (a, b, c) where
  fragmentize (a, b, c) = singleton @(a, b, c) (a, b, c) <> fragmentize a <> fragmentize b <> fragmentize c

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d) => Fragmentable (a, b, c, d) where
  fragmentize (a, b, c, d) = singleton @(a, b, c, d) (a, b, c, d) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e) => Fragmentable (a, b, c, d, e) where
  fragmentize (a, b, c, d, e) = singleton @(a, b, c, d, e) (a, b, c, d, e) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f) => Fragmentable (a, b, c, d, e, f) where
  fragmentize (a, b, c, d, e, f) = singleton @(a, b, c, d, e, f) (a, b, c, d, e, f) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g) => Fragmentable (a, b, c, d, e, f, g) where
  fragmentize (a, b, c, d, e, f, g) = singleton @(a, b, c, d, e, f, g) (a, b, c, d, e, f, g) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g, Fragmentable h) => Fragmentable (a, b, c, d, e, f, g, h) where
  fragmentize (a, b, c, d, e, f, g, h) = singleton @(a, b, c, d, e, f, g, h) (a, b, c, d, e, f, g, h) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g <> fragmentize h

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g, Fragmentable h, Fragmentable i) => Fragmentable (a, b, c, d, e, f, g, h, i) where
  fragmentize (a, b, c, d, e, f, g, h, i) = singleton @(a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g, h, i) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g <> fragmentize h <> fragmentize i

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g, Fragmentable h, Fragmentable i, Fragmentable j) => Fragmentable (a, b, c, d, e, f, g, h, i, j) where
  fragmentize (a, b, c, d, e, f, g, h, i, j) = singleton @(a, b, c, d, e, f, g, h, i, j) (a, b, c, d, e, f, g, h, i, j) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g <> fragmentize h <> fragmentize i <> fragmentize j
