{-# LANGUAGE RankNTypes #-}
module Test.Mutagen.Test.Batch where

import Control.Monad.Extra (concatMapM)

import Test.Mutagen.Mutation

----------------------------------------
-- Mutation batches

data MutationBatch args = MutationBatch
  { mb_value :: args
  , mb_order :: MutationOrder
  , mb_size :: Int
  , mb_test_passed :: Bool
  , mb_past_pos :: [Pos]
  , mb_next_pos :: [Pos]
  , mb_nrand :: Int
  , mb_curr_queue :: [args]
  , mb_nmuts :: Int
  }

newMutationBatch :: Mutable a => MutationOrder -> Int -> Int -> Int -> Maybe [Pos] -> Bool -> a -> MutationBatch a
newMutationBatch order nrand size nmuts pos passed args =
  MutationBatch
  { mb_value = args
  , mb_next_pos = maybe (order (positions args)) id pos
  , mb_past_pos = mempty
  , mb_test_passed = passed
  , mb_curr_queue = mempty
  , mb_nmuts = nmuts
  , mb_order = order
  , mb_size = size
  , mb_nrand = nrand
  }

newMutationBatchFromParent :: Mutable a => MutationBatch a -> Maybe [Pos] -> Bool -> a -> MutationBatch a
newMutationBatchFromParent mb pos passed args =
  mb
  { mb_value = args
  , mb_next_pos = maybe (mb_order mb (positions args)) id pos
  , mb_past_pos = mempty
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
