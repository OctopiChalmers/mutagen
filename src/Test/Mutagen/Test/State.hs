{-# LANGUAGE RankNTypes #-}
module Test.Mutagen.Test.State where

import Data.PQueue.Prio.Min (MinPQueue)
import Data.Time.Clock.POSIX

import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Random (QCGen, newQCGen)

import Tracer

import Test.Mutagen.Property
import Test.Mutagen.Mutation
import Test.Mutagen.Fragment
import Test.Mutagen.Test.Config
import Test.Mutagen.Test.Batch

----------------------------------------
-- | Testing state (internal)

data State log
  = State
  -- Static
  { stMaxTests          :: Int
  , stMaxDiscardRatio   :: Int
  , stMaxSize           :: Int
  , stArgsGen           :: Gen Args
  , stArgsRunner        :: Args -> Result
  , stDebug             :: Bool
  , stStepByStep        :: Bool
  , stTimeout           :: (Maybe Integer)
  , stStartTime         :: Integer
  , stRandomMutations   :: Int
  , stRandomFragments   :: Int
  , stMutationLimit     :: Int
  , stResetAfter        :: Maybe Int
  , stMutationOrder     :: MutationOrder
  , stMaxTraceLength    :: Int
  , stUseFragments      :: !Bool
  -- Dynamic
  , stUsedSeed          :: !(Maybe (QCGen, Int))
  , stNextSeed          :: !QCGen
  , stNumExGenMut       :: !(Int, Int, Int, Int)
  , stNumInteresting    :: !Int
  , stLastInteresting   :: !Int
  , stTraceLogResets    :: !Int
  , stNumPassed         :: !Int
  , stPassedTraceLog    :: !log
  , stPassedQueue       :: !MutationQueue
  , stNumDiscarded      :: !Int
  , stDiscardedTraceLog :: !log
  , stDiscardedQueue    :: !MutationQueue
  , stNumTraceNodes     :: !Int
  , stFragmentStore     :: !FragmentStore
  }

createInitialState :: forall log. TraceLogger log => Config -> Property -> IO (State log)
createInitialState cfg (Property gen argsRunner) = do
  -- rng generator
  rng <- newQCGen
  -- start timestamp
  now <- round <$> getPOSIXTime
  -- the initial internal state
  traceNodes <- read <$> readFile ".tracer"
  putStrLn ("Read trace nodes count from .tracer: " <> show traceNodes)
  passedTraceLog <- emptyTraceLog traceNodes
  discardedTraceLog <- emptyTraceLog traceNodes
  return State
    { stMaxTests = maxSuccess cfg
    , stMaxDiscardRatio = maxDiscardRatio cfg
    , stMaxSize = maxSize cfg
    , stArgsGen = gen
    , stArgsRunner = argsRunner
    , stNextSeed = rng
    , stUsedSeed = Nothing
    , stDebug = chatty cfg
    , stStepByStep = stepByStep cfg
    , stTimeout = timeout cfg
    , stStartTime = now
    , stMaxTraceLength = maxTraceLength cfg
    , stUseFragments = useFragments cfg
    , stRandomMutations = randomMutations cfg
    , stRandomFragments = randomFragments cfg
    , stMutationLimit = maybe (maxSize cfg) id (mutationLimit cfg)
    , stResetAfter = resetAfter cfg
    , stMutationOrder = mutationOrder cfg
    , stNumExGenMut = (0,0,0,0)
    , stNumInteresting = 0
    , stLastInteresting = 0
    , stTraceLogResets = 0
    , stNumPassed = 0
    , stPassedTraceLog = passedTraceLog
    , stPassedQueue = mempty
    , stNumDiscarded = 0
    , stDiscardedTraceLog = discardedTraceLog
    , stDiscardedQueue = mempty
    , stNumTraceNodes = traceNodes
    , stFragmentStore = foldr storeFragments emptyFragmentStore (examples cfg)
    }

----------------------------------------
-- Mutation priority queues

-- Mutation candidates
type MutationQueue =
  MinPQueue
    Int                  -- The candidate priority
    ( Args               -- The test case inputs
    , Trace              -- The code coverage it triggered
    , MutationBatch Args -- The batch of possible mutations
    )

-- | Inherit the mutation batch of a parent test case of it exists, otherwise create a new one.
createOrInheritMutationBatch :: State log -> Args -> Maybe (MutationBatch Args) -> Maybe [Pos] -> Bool -> MutationBatch Args
createOrInheritMutationBatch st args parentbatch pos isPassed =
  case parentbatch of
    -- test case was mutated from an existing one, we can augment its parent mutation batch
    Just mb ->
      newMutationBatchFromParent mb
        pos isPassed args
    -- test case was freshly generated, we need to initialize a new mutation batch
    Nothing ->
      newMutationBatch
        (stMutationOrder st)
        (stRandomMutations st)
        (stMaxSize st)
        (stRandomFragments st)
        (stMutationLimit st)
        pos isPassed args

----------------------------------------
-- State-related utilities

-- | Check whether the timeout has passed
passedTimeout :: State log -> IO Bool
passedTimeout st
  | Just s <- stTimeout st = do
      now <- round <$> getPOSIXTime
      return (now >= stStartTime st + s)
  | otherwise = return False

computeSize :: State log -> Int
computeSize st
  | stNumPassed st `roundTo` stMaxSize st + stMaxSize st <= stMaxTests st
    || stNumPassed st >= stMaxTests st
    || stMaxTests st `mod` stMaxSize st == 0 =
    (stNumPassed st `mod` stMaxSize st + stNumDiscarded st `div` 10) `min` stMaxSize st
  | otherwise =
      ((stNumPassed st `mod` stMaxSize st) * stMaxSize st
       `div` (stMaxTests st `mod` stMaxSize st) + stNumDiscarded st `div` 10) `min` stMaxSize st

roundTo :: Integral a => a -> a -> a
roundTo n m = (n `div` m) * m

at0 :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
at0 _ s 0 0 = s
at0 f _ n d = f n d
