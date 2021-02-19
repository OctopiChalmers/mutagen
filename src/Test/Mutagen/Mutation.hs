{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Mutagen.Mutation where

import Control.Monad
import Control.Monad.Extra

import Data.List
import Data.Tree

import Test.QuickCheck

-- For providing some default Mutable instances
import Data.Char
import Data.Word

----------------------------------------
-- | Breadcrumbs used to place mutations inside of values

type Pos = [Int]

----------------------------------------
-- | Mutants

data Mutant a = Pure a | Random (Gen a)

instance Show (Mutant a) where
  show (Pure _)   = "{PURE}"
  show (Random _) = "{RANDOM}"

instance Functor Mutant where
  fmap f (Pure a) = Pure (f a)
  fmap f (Random gen) = Random (fmap f gen)

concretize :: Int -> Int -> Mutant a -> IO [a]
concretize _ _ (Pure a)     = return [a]
concretize n s (Random gen) = replicateM n (generate (resize s gen))

----------------------------------------
-- | Mutations as transformations of values into mutants

type Mutation a = a -> [Mutant a]

(|+|) :: Mutation a -> Mutation a -> Mutation a
(|+|) f g x = f x <> g x

----------------------------------------
-- | Mutable types

type GenericMutation = forall m. Mutable m => Mutation m

class Mutable a where
  -- The default value of the type to be used when mutating
  def :: a
  def = defaultDef
  -- Top-level constructor mutations
  mutate :: Mutation a
  mutate = defaultMutate
  -- List all the possible positions where we could mutate the value
  positions :: a -> Tree Pos
  positions = defaultPositions
  -- Apply top-level mutations inside a value
  inside :: Pos -> GenericMutation -> Mutation a
  inside = defaultInside

-- Join all possible mutations, top-level and nested
mutateEverywhere :: Mutable a => Mutation a
mutateEverywhere a = mutate a <> mutateInside a
  where
    mutateInside = mconcat [ inside pos mutate | pos <- levelorder (positions a) ]


----------------------------------------
-- Instance declaration helpers

-- Default method implementations

defaultDef :: a
defaultDef = error "def: not defined"

defaultMutate :: Mutation a
defaultMutate = mempty

defaultPositions :: a -> Tree Pos
defaultPositions _ = node []

defaultInside :: Mutable a => Pos -> GenericMutation -> Mutation a
defaultInside []  mut = mut
defaultInside pos _   = invalidPosition pos

-- For defining inside

wrap :: [Mutant a] -> (a -> b) -> [Mutant b]
wrap mutants wrapper = fmap (fmap wrapper) mutants

-- For defining positions

node :: [(Int, Tree Pos)] -> Tree Pos
node xs = Node [] (fmap (\(idx, children) -> fmap (idx:) children) xs)

invalidPosition :: Pos -> a
invalidPosition pos = error ("inside: invalid position: " <> show pos)

invalidPositionShow :: Show a => Pos -> a -> b
invalidPositionShow pos a = error ("inside: invalid position: " <> show pos <> "\nvalue: " <> show a)


----------------------------------------
-- | Mutation order

-- Mutation order (default: levelorder)
type MutationOrder = forall a. Tree a -> [a]

preorder :: MutationOrder
preorder t = squish t []
  where squish (Node x ts) xs = x : foldr squish xs ts

postorder :: MutationOrder
postorder t = squish [] t
  where squish xs (Node x ts) = x : foldl' squish xs ts

levelorder :: MutationOrder
levelorder = concat . levels

----------------------------------------
-- | Mutation batches

data MutationBatch a = MutationBatch
  { mb_value :: a
  , mb_order :: MutationOrder
  , mb_size :: Int
  , mb_test_passed :: Bool
  , mb_past_pos :: [Pos]
  , mb_next_pos :: [Pos]
  , mb_nrand :: Int
  , mb_curr_queue :: [a]
  , mb_nmuts :: Int
  }

newMutationBatch :: Mutable a => MutationOrder -> Int -> Int -> Int -> Bool -> a -> MutationBatch a
newMutationBatch order nrand size nmuts passed a =
  MutationBatch
  { mb_value = a
  , mb_order = order
  , mb_size = size
  , mb_test_passed = passed
  , mb_nrand = nrand
  , mb_past_pos = mempty
  , mb_next_pos = order (positions a)
  , mb_curr_queue = mempty
  , mb_nmuts = nmuts
  }

newMutationBatchFromParent :: Mutable a => MutationBatch a -> Bool -> a  -> MutationBatch a
newMutationBatchFromParent mb passed a =
  let (prevPos, newPos) = partition (`elem` mb_past_pos mb) (mb_order mb (positions a))
  in mb { mb_value = a
        , mb_next_pos = newPos
        , mb_past_pos =
            if mb_test_passed mb
            then reverse prevPos
            else mempty
        , mb_test_passed = passed
        , mb_curr_queue = mempty
        , mb_nmuts = mb_nmuts mb - 1
        }

nextMutation :: Mutable a => MutationBatch a -> IO (Maybe (a, MutationBatch a))
nextMutation mb | mb_nmuts mb == 0 = return Nothing -- too many mutations
nextMutation mb = do
  case mb_curr_queue mb of
    -- queue is empty, advance to next position
    [] -> do
      case mb_next_pos mb of
        -- no more positions to mutate
        [] -> return Nothing
        -- next position available
        (pos:ps) -> do
          let mutants = inside pos mutate (mb_value mb)
          queue <- concatMapM (concretize (mb_nrand mb) (mb_size mb)) mutants
          case queue of
            -- current position admits no mutations: advance to next position
            [] -> do
              let mb' = mb { mb_next_pos = ps
                           , mb_past_pos = pos : mb_past_pos mb
                           }
              nextMutation mb'
            -- current position admits some mutations: update the batch queue
            -- and lock the current position
            (a:as) -> do
              let mb' = mb { mb_next_pos = ps
                           , mb_past_pos = pos : mb_past_pos mb
                           , mb_curr_queue = as
                           }
              return (Just (a, mb'))
    -- there are some mutants still in the queue for the current position
    (a:as) -> do
      return (Just (a, mb { mb_curr_queue = as }))

----------------------------------------
-- | Mutable Instances
----------------------------------------

----------------------------------------
-- | A mutable wrapper that produces no mutations
-- Useful for fullfiling the mutable constraint on some input arguments

newtype Rigid a = Rigid a
  deriving (Show, Eq, Ord, Read)

unRigid :: Rigid a -> a
unRigid (Rigid a) = a

instance Arbitrary a => Arbitrary (Rigid a) where
  arbitrary = Rigid <$> arbitrary

instance Arbitrary a => Mutable (Rigid a)

----------------------------------------
-- | Base types instances

instance Mutable Int where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Double where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Float where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Word8 where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Word16 where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Word32 where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Word64 where
  def = 0
  mutate _ = [ Random arbitrary ]

instance Mutable Char where
  def = chr 0
  mutate = const [ Random arbitrary ]

instance Mutable Bool where
  def = False
  mutate b = [ Pure (not b) ]

instance (Arbitrary a, Mutable a) => Mutable (Maybe a) where
  def = Nothing

  mutate Nothing  = [ Pure (Just def) ]
  mutate (Just _) = [ Pure Nothing ]

  positions Nothing  = node []
  positions (Just a) = node [ (0, positions a) ]

  inside []     mut x        = mut x
  inside (0:ps) mut (Just a) = wrap (inside ps mut a) (\x -> Just x)
  inside pos    _   _        = invalidPosition pos


instance (Arbitrary a, Mutable a) => Mutable [a] where
  def = []

  mutate [] =
    [ Pure [def]
    , Random $ sized $ \s -> do
        n <- choose (1,s `div` 2)
        vectorOf n arbitrary
    ]
  mutate [x] =
    [ Pure [], Pure [x,x]
    , Random $ sized $ \s -> do
        n <- choose (1,s `div` 2)
        y <- vectorOf n arbitrary
        return (x:y)
    ]
  mutate (x:xs) =
    [ Pure [], Pure xs, Pure (x:x:xs)
    , Random $ sized $ \s -> do
        n <- choose (1,s `div` 2)
        y <- vectorOf n arbitrary
        return (x:y<>xs)
    ]

  positions []     = node []
  positions (x:xs) = node [ (0, positions x), (1, positions xs) ]

  inside []     mut xs     = mut xs
  inside (0:ps) mut (a:as) = wrap (inside ps mut a) (\x -> x:as)
  inside (1:ps) mut (a:as) = wrap (inside ps mut as) (\xs -> a:xs)
  inside pos    _   _      = invalidPosition pos


-- Tuple instances

instance (Mutable a, Mutable b) => Mutable (a, b) where
  def = (def, def)
  mutate (a, b) =
    [ fmap (\x -> (x, b)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x)) gb | gb <- mutate b ]

  positions (a, b) =
    node [ (0, positions a), (1, positions b) ]

  inside []     mut x      = mut x
  inside (0:ps) mut (a, b) = wrap (inside ps mut a) (\x -> (x, b))
  inside (1:ps) mut (a, b) = wrap (inside ps mut b) (\x -> (a, x))
  inside pos    _   _      = invalidPosition pos


instance (Mutable a, Mutable b, Mutable c) => Mutable (a, b, c) where
  def = (def, def, def)

  mutate (a, b, c) =
    [ fmap (\x -> (x, b, c)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x)) gc | gc <- mutate c ]

  positions (a, b, c) =
    node [ (0, positions a), (1, positions b), (2, positions c) ]

  inside []     mut x         = mut x
  inside (0:ps) mut (a, b, c) = wrap (inside ps mut a) (\x -> (x, b, c))
  inside (1:ps) mut (a, b, c) = wrap (inside ps mut b) (\x -> (a, x, c))
  inside (2:ps) mut (a, b, c) = wrap (inside ps mut c) (\x -> (a, b, x))
  inside pos    _   _         = invalidPosition pos

instance (Mutable a, Mutable b, Mutable c, Mutable d) => Mutable (a, b, c, d) where
  def = (def, def, def, def)

  mutate (a, b, c, d) =
    [ fmap (\x -> (x, b, c, d)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x)) gd | gd <- mutate d ]

  positions (a, b, c, d) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d) ]

  inside []     mut x            = mut x
  inside (0:ps) mut (a, b, c, d) = wrap (inside ps mut a) (\x -> (x, b, c, d))
  inside (1:ps) mut (a, b, c, d) = wrap (inside ps mut b) (\x -> (a, x, c, d))
  inside (2:ps) mut (a, b, c, d) = wrap (inside ps mut c) (\x -> (a, b, x, d))
  inside (3:ps) mut (a, b, c, d) = wrap (inside ps mut d) (\x -> (a, b, c, x))
  inside pos    _   _            = invalidPosition pos

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e) => Mutable (a, b, c, d, e) where
  def = (def, def, def, def, def)

  mutate (a, b, c, d, e) =
    [ fmap (\x -> (x, b, c, d, e)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d, e)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d, e)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x, e)) gd | gd <- mutate d ] <>
    [ fmap (\x -> (a, b, c, d, x)) ge | ge <- mutate e ]

  positions (a, b, c, d, e) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d), (4, positions e) ]

  inside []     mut x               = mut x
  inside (0:ps) mut (a, b, c, d, e) = wrap (inside ps mut a) (\x -> (x, b, c, d, e))
  inside (1:ps) mut (a, b, c, d, e) = wrap (inside ps mut b) (\x -> (a, x, c, d, e))
  inside (2:ps) mut (a, b, c, d, e) = wrap (inside ps mut c) (\x -> (a, b, x, d, e))
  inside (3:ps) mut (a, b, c, d, e) = wrap (inside ps mut d) (\x -> (a, b, c, x, e))
  inside (4:ps) mut (a, b, c, d, e) = wrap (inside ps mut e) (\x -> (a, b, c, d, x))
  inside pos    _   _               = invalidPosition pos

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f) => Mutable (a, b, c, d, e, f) where
  def = (def, def, def, def, def, def)

  mutate (a, b, c, d, e, f) =
    [ fmap (\x -> (x, b, c, d, e, f)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d, e, f)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d, e, f)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x, e, f)) gd | gd <- mutate d ] <>
    [ fmap (\x -> (a, b, c, d, x, f)) ge | ge <- mutate e ] <>
    [ fmap (\x -> (a, b, c, d, e, x)) gf | gf <- mutate f ]

  positions (a, b, c, d, e, f) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d), (4, positions e)
         , (5, positions f) ]

  inside []     mut x                  = mut x
  inside (0:ps) mut (a, b, c, d, e, f) = wrap (inside ps mut a) (\x -> (x, b, c, d, e, f))
  inside (1:ps) mut (a, b, c, d, e, f) = wrap (inside ps mut b) (\x -> (a, x, c, d, e, f))
  inside (2:ps) mut (a, b, c, d, e, f) = wrap (inside ps mut c) (\x -> (a, b, x, d, e, f))
  inside (3:ps) mut (a, b, c, d, e, f) = wrap (inside ps mut d) (\x -> (a, b, c, x, e, f))
  inside (4:ps) mut (a, b, c, d, e, f) = wrap (inside ps mut e) (\x -> (a, b, c, d, x, f))
  inside (5:ps) mut (a, b, c, d, e, f) = wrap (inside ps mut f) (\x -> (a, b, c, d, e, x))
  inside pos    _   _                  = invalidPosition pos

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f, Mutable g) => Mutable (a, b, c, d, e, f, g) where
  def = (def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g) =
    [ fmap (\x -> (x, b, c, d, e, f, g)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d, e, f, g)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d, e, f, g)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x, e, f, g)) gd | gd <- mutate d ] <>
    [ fmap (\x -> (a, b, c, d, x, f, g)) ge | ge <- mutate e ] <>
    [ fmap (\x -> (a, b, c, d, e, x, g)) gf | gf <- mutate f ] <>
    [ fmap (\x -> (a, b, c, d, e, f, x)) gg | gg <- mutate g ]

  positions (a, b, c, d, e, f, g) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d), (4, positions e)
         , (5, positions f), (6, positions g) ]

  inside []     mut x                     = mut x
  inside (0:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut a) (\x -> (x, b, c, d, e, f, g))
  inside (1:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut b) (\x -> (a, x, c, d, e, f, g))
  inside (2:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut c) (\x -> (a, b, x, d, e, f, g))
  inside (3:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut d) (\x -> (a, b, c, x, e, f, g))
  inside (4:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut e) (\x -> (a, b, c, d, x, f, g))
  inside (5:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut f) (\x -> (a, b, c, d, e, x, g))
  inside (6:ps) mut (a, b, c, d, e, f, g) = wrap (inside ps mut g) (\x -> (a, b, c, d, e, f, x))
  inside pos    _   _                     = invalidPosition pos

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f, Mutable g, Mutable h) => Mutable (a, b, c, d, e, f, g, h) where
  def = (def, def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g, h) =
    [ fmap (\x -> (x, b, c, d, e, f, g, h)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d, e, f, g, h)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d, e, f, g, h)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x, e, f, g, h)) gd | gd <- mutate d ] <>
    [ fmap (\x -> (a, b, c, d, x, f, g, h)) ge | ge <- mutate e ] <>
    [ fmap (\x -> (a, b, c, d, e, x, g, h)) gf | gf <- mutate f ] <>
    [ fmap (\x -> (a, b, c, d, e, f, x, h)) gg | gg <- mutate g ] <>
    [ fmap (\x -> (a, b, c, d, e, f, g, x)) gh | gh <- mutate h ]

  positions (a, b, c, d, e, f, g, h) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d), (4, positions e)
         , (5, positions f), (6, positions g), (7, positions h) ]

  inside []     mut x                        = mut x
  inside (0:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut a) (\x -> (x, b, c, d, e, f, g, h))
  inside (1:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut b) (\x -> (a, x, c, d, e, f, g, h))
  inside (2:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut c) (\x -> (a, b, x, d, e, f, g, h))
  inside (3:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut d) (\x -> (a, b, c, x, e, f, g, h))
  inside (4:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut e) (\x -> (a, b, c, d, x, f, g, h))
  inside (5:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut f) (\x -> (a, b, c, d, e, x, g, h))
  inside (6:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut g) (\x -> (a, b, c, d, e, f, x, h))
  inside (7:ps) mut (a, b, c, d, e, f, g, h) = wrap (inside ps mut h) (\x -> (a, b, c, d, e, f, g, x))
  inside pos    _   _                        = invalidPosition pos


instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f, Mutable g, Mutable h, Mutable i) => Mutable (a, b, c, d, e, f, g, h, i) where
  def = (def, def, def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g, h, i) =
    [ fmap (\x -> (x, b, c, d, e, f, g, h, i)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d, e, f, g, h, i)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d, e, f, g, h, i)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x, e, f, g, h, i)) gd | gd <- mutate d ] <>
    [ fmap (\x -> (a, b, c, d, x, f, g, h, i)) ge | ge <- mutate e ] <>
    [ fmap (\x -> (a, b, c, d, e, x, g, h, i)) gf | gf <- mutate f ] <>
    [ fmap (\x -> (a, b, c, d, e, f, x, h, i)) gg | gg <- mutate g ] <>
    [ fmap (\x -> (a, b, c, d, e, f, g, x, i)) gh | gh <- mutate h ] <>
    [ fmap (\x -> (a, b, c, d, e, f, g, h, x)) gi | gi <- mutate i ]

  positions (a, b, c, d, e, f, g, h, i) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d), (4, positions e)
         , (5, positions f), (6, positions g), (7, positions h), (8, positions i) ]

  inside []     mut x                           = mut x
  inside (0:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut a) (\x -> (x, b, c, d, e, f, g, h, i))
  inside (1:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut b) (\x -> (a, x, c, d, e, f, g, h, i))
  inside (2:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut c) (\x -> (a, b, x, d, e, f, g, h, i))
  inside (3:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut d) (\x -> (a, b, c, x, e, f, g, h, i))
  inside (4:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut e) (\x -> (a, b, c, d, x, f, g, h, i))
  inside (5:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut f) (\x -> (a, b, c, d, e, x, g, h, i))
  inside (6:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut g) (\x -> (a, b, c, d, e, f, x, h, i))
  inside (7:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut h) (\x -> (a, b, c, d, e, f, g, x, i))
  inside (8:ps) mut (a, b, c, d, e, f, g, h, i) = wrap (inside ps mut i) (\x -> (a, b, c, d, e, f, g, h, x))
  inside pos    _   _                           = invalidPosition pos

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f, Mutable g, Mutable h, Mutable i, Mutable j) => Mutable (a, b, c, d, e, f, g, h, i, j) where
  def = (def, def, def, def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g, h, i, j) =
    [ fmap (\x -> (x, b, c, d, e, f, g, h, i, j)) ga | ga <- mutate a ] <>
    [ fmap (\x -> (a, x, c, d, e, f, g, h, i, j)) gb | gb <- mutate b ] <>
    [ fmap (\x -> (a, b, x, d, e, f, g, h, i, j)) gc | gc <- mutate c ] <>
    [ fmap (\x -> (a, b, c, x, e, f, g, h, i, j)) gd | gd <- mutate d ] <>
    [ fmap (\x -> (a, b, c, d, x, f, g, h, i, j)) ge | ge <- mutate e ] <>
    [ fmap (\x -> (a, b, c, d, e, x, g, h, i, j)) gf | gf <- mutate f ] <>
    [ fmap (\x -> (a, b, c, d, e, f, x, h, i, j)) gg | gg <- mutate g ] <>
    [ fmap (\x -> (a, b, c, d, e, f, g, x, i, j)) gh | gh <- mutate h ] <>
    [ fmap (\x -> (a, b, c, d, e, f, g, h, x, j)) gi | gi <- mutate i ] <>
    [ fmap (\x -> (a, b, c, d, e, f, g, h, i, x)) gj | gj <- mutate j ]

  positions (a, b, c, d, e, f, g, h, i, j) =
    node [ (0, positions a), (1, positions b), (2, positions c), (3, positions d), (4, positions e)
         , (5, positions f), (6, positions g), (7, positions h), (8, positions i), (9, positions j) ]

  inside []     mut x                              = mut x
  inside (0:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut a) (\x -> (x, b, c, d, e, f, g, h, i, j))
  inside (1:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut b) (\x -> (a, x, c, d, e, f, g, h, i, j))
  inside (2:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut c) (\x -> (a, b, x, d, e, f, g, h, i, j))
  inside (3:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut d) (\x -> (a, b, c, x, e, f, g, h, i, j))
  inside (4:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut e) (\x -> (a, b, c, d, x, f, g, h, i, j))
  inside (5:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut f) (\x -> (a, b, c, d, e, x, g, h, i, j))
  inside (6:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut g) (\x -> (a, b, c, d, e, f, x, h, i, j))
  inside (7:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut h) (\x -> (a, b, c, d, e, f, g, x, i, j))
  inside (8:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut i) (\x -> (a, b, c, d, e, f, g, h, x, j))
  inside (9:ps) mut (a, b, c, d, e, f, g, h, i, j) = wrap (inside ps mut j) (\x -> (a, b, c, d, e, f, g, h, i, x))
  inside pos    _   _                              = invalidPosition pos
