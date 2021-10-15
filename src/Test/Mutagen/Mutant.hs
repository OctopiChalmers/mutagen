module Test.Mutagen.Mutant where

import Control.Monad

import Data.Typeable

import Test.QuickCheck

import Test.Mutagen.Fragment

----------------------------------------
-- | Mutants

data Mutant a =
    Pure a
  | Rand (Gen a)
  | Frag (FragmentStore -> Gen [a])

instance Show (Mutant a) where
  show (Pure _) = "Pure(..)"
  show (Rand _) = "Rand(..)"
  show (Frag _) = "Frag(..)"

instance Functor Mutant where
  fmap f (Pure mut) = Pure (f mut)
  fmap f (Rand gen) = Rand (fmap f gen)
  fmap f (Frag fun) = Frag (fmap (fmap (fmap f)) fun)

data MutantKind = PureMutant | RandMutant | FragMutant

data Concretized a = Concretized MutantKind a

concretize :: Typeable a => (Int, Int) -> (Int, FragmentStore) -> Mutant a -> IO [Concretized a]
concretize _ _ (Pure mut) = do
  fmap (Concretized PureMutant) <$> return [mut]
concretize (n, s) _ (Rand gen) = do
  fmap (Concretized RandMutant) <$> replicateM n  (generate (resize s gen))
concretize _ (n, fs) (Frag fun) = do
  fmap (Concretized FragMutant) <$> fmap (take n) (generate (fun fs))
