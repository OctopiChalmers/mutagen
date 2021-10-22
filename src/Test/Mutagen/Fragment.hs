{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Mutagen.Fragment where

import Control.Monad

import Data.Maybe
import Data.Typeable

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck

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
  show (Fragment a) = "Fragment(" <> show a <> ")"

----------------------------------------
-- Storing fragments based on their type

newtype FragmentStore = FragmentStore (Map TypeRep (Set Fragment))

fragmentStoreSize :: FragmentStore -> [(TypeRep, Int)]
fragmentStoreSize (FragmentStore fs) =
  [ (tr, Set.size frags)
  | (tr, frags) <- Map.toList fs
  ]

instance Semigroup FragmentStore where
  FragmentStore fs1 <> FragmentStore fs2 =
    FragmentStore (Map.unionWith Set.union fs1 fs2)

instance Monoid FragmentStore where
  mempty = emptyFragmentStore

printFragmentStore :: FragmentStore -> IO ()
printFragmentStore (FragmentStore fs) = do
  forM_ (Map.assocs fs) $ \(tr, frags) -> do
    putStrLn ("TypeRep: " <> show tr)
    forM_ frags $ \frag -> do
      putStrLn ("* " <> show frag)

emptyFragmentStore :: FragmentStore
emptyFragmentStore = FragmentStore mempty

insertFragment :: Maybe [TypeRep] -> TypeRep -> Fragment -> FragmentStore -> FragmentStore
insertFragment Nothing tr fr (FragmentStore fs) =
  FragmentStore (Map.insertWith Set.union tr (Set.singleton fr) fs)
insertFragment (Just trs) tr fr (FragmentStore fs)
  | tr `elem` trs = FragmentStore (Map.insertWith Set.union tr (Set.singleton fr) fs)
  | otherwise     = FragmentStore fs

collectFragments :: Fragmentable a => Maybe [TypeRep] -> a -> FragmentStore
collectFragments allowed a = foldr (uncurry (insertFragment allowed)) emptyFragmentStore fts
  where
    fts = Set.map (\(Fragment x) -> (typeOf x, Fragment x)) (fragmentize a)

storeFragments :: Fragmentable a => Maybe [TypeRep] -> a -> FragmentStore -> FragmentStore
storeFragments allowed a fs = fs <> collectFragments allowed a

sampleFragments :: Typeable a => a -> FragmentStore -> Gen [a]
sampleFragments a (FragmentStore fs) = do
  case Map.lookup (typeOf a) fs of
    Nothing ->
      return []
    Just frags ->
      catMaybes . fmap (\(Fragment a') -> cast a') <$> shuffle (Set.toList frags)

-- sampleFragment :: TypeRep -> FragmentStore -> IO (Maybe Fragment)
-- sampleFragment tr (FragmentStore fs) = do
--   case Map.lookup tr fs of
--     Nothing -> return Nothing
--     Just frags -> Just <$> generate (elements (Set.toList frags))

----------------------------------------
-- Fragmentizing values

class IsFragment a => Fragmentable a where
  fragmentize :: a -> Set Fragment
  fragmentize = singleton

-- Helpers

singleton :: Fragmentable a => a -> Set Fragment
singleton = Set.singleton . Fragment

----------------------------------------
-- Replacing a subexpression within a value with a random fragmentize

-- replacePosWithFragment :: (Mutable a, Fragmentable a) => FragmentStore -> Pos -> Mutation a
-- replacePosWithFragment (FragmentStore fs) pos = inside pos $ \a ->
--   case Map.lookup (typeOf a) fs of
--     Nothing -> []
--     Just frags ->
--       [ Random $ do
--           Fragment a' <- elements (Set.toList frags)
--           return (maybe a id (cast a'))
--       ]


----------------------------------------
-- Fragmentable instances
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
  fragmentize x =
    case x of
      Nothing -> singleton x
      Just v1 -> singleton x <> fragmentize v1

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
