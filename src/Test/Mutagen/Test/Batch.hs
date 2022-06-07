{-# LANGUAGE RankNTypes #-}

module Test.Mutagen.Test.Batch where

import Control.Monad.Extra (concatMapM)

import Data.Maybe

import Test.Mutagen.Mutant
import Test.Mutagen.Mutation
import Test.Mutagen.Fragment

----------------------------------------
-- Mutation batches

data MutationBatch args = MutationBatch
  { mb_value :: args
  , mb_past_pos :: [Pos]
  , mb_next_pos :: [Pos]
  , mb_curr_queue :: [Concretized args]
  , mb_mut_order :: MutationOrder
  , mb_test_passed :: Bool
  , mb_rand_size :: Int
  , mb_rand_num :: Int
  , mb_frag_num :: Int
  , mb_mut_lim :: Int
  }

newMutationBatch :: Mutable a => MutationOrder -> Int -> Int -> Int -> Int -> Maybe [Pos] -> Bool -> a -> MutationBatch a
newMutationBatch mut_order rand_mut_num rand_mut_size frag_num mut_lim eval_pos test_passed args =
  MutationBatch
  { mb_value = args
  , mb_past_pos = mempty
  , mb_next_pos = fromMaybe (mut_order (positions args)) eval_pos
  , mb_curr_queue = mempty
  , mb_mut_order = mut_order
  , mb_test_passed = test_passed
  , mb_rand_size = rand_mut_size
  , mb_rand_num = rand_mut_num
  , mb_mut_lim = mut_lim
  , mb_frag_num = frag_num
  }

newMutationBatchFromParent :: Mutable a => MutationBatch a -> Maybe [Pos] -> Bool -> a -> MutationBatch a
newMutationBatchFromParent mb pos test_passed args =
  mb
  { mb_value = args
  , mb_next_pos = fromMaybe (mb_mut_order mb (positions args)) pos
  , mb_past_pos = mempty
  , mb_test_passed = test_passed
  , mb_curr_queue = mempty
  , mb_mut_lim = mb_mut_lim mb - 1
  }

nextMutation :: Mutable a => FragmentStore -> MutationBatch a -> IO (Maybe (a, MutantKind, MutationBatch a))
nextMutation _  mb | mb_mut_lim mb == 0 = return Nothing -- too many mutations
nextMutation fs mb = do
  case mb_curr_queue mb of
    -- Queue is empty, advance to next position
    [] -> do
      case mb_next_pos mb of
        -- No more positions to mutate
        [] -> return Nothing
        -- Next position available
        pos : ps -> do
          let mutants = inside pos mutate (mb_value mb)
          queue <- concatMapM (concretize (mb_rand_num mb, mb_rand_size mb) (mb_rand_num mb, fs)) mutants
          case queue of
            -- Current position admits no mutations: advance to next position
            [] -> do
              let mb' = mb { mb_next_pos = ps
                           , mb_past_pos = pos : mb_past_pos mb
                           }
              nextMutation fs mb'
            -- Current position admits some mutations: update the batch queue
            -- and lock the current position
            Concretized mk a : as -> do
              let mb' = mb { mb_next_pos = ps
                           , mb_past_pos = pos : mb_past_pos mb
                           , mb_curr_queue = as
                           }
              return (Just (a, mk, mb'))
    -- There are some mutants still in the queue for the current position
    Concretized mk a : as -> do
      return (Just (a, mk, mb { mb_curr_queue = as }))
