module Test.Mutagen.Test.Loop where

import Control.Monad
import Control.Monad.Extra (ifM)
import Text.Printf

import qualified Data.PQueue.Prio.Min as PQueue

import System.IO
import System.Random (split)

import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (QCGen)

import Tracer
import Test.Mutagen.Property
import Test.Mutagen.Lazy
import Test.Mutagen.Test.State
import Test.Mutagen.Test.Batch
import Test.Mutagen.Test.Report
import Test.Mutagen.Test.Terminal

----------------------------------------
-- | Replay a counterexample

replayCounterexample :: Property -> (QCGen, Int) -> IO Report
replayCounterexample p (rng, size) = do
  let Property genArgs runTest = property p
  let args = unGen genArgs rng size
  test <- unResult (protectResult (runTest args))
  case test of
    Failed -> do
      printf "Replayed test failed...\n"
      reportCounterexample args test
      return Counterexample
        { numPassed = 0
        , numDiscarded = 0
        , failingArgs = args
        , failingSeed = Just (rng, size)
        }
    Passed -> do
      printf "Replayed test passed...\n"
      return AllPassed
        { numPassed = 1
        , numDiscarded = 0
        }
    Discarded -> do
      printf "Replayed test got discarded...\n"
      return AllPassed
        { numPassed = 0
        , numDiscarded = 1
        }

----------------------------------------
-- | The main test loop

-- We abstract the test case runner based on the way it logs execution traces
type TestCaseRunner log = State log -> Maybe (MutationBatch Args) -> Args -> IO (Either Test (State log))

loop :: TraceLogger log => TestCaseRunner log -> State log -> IO Report
loop runner st
  -- we reached the max number of tests
  | stNumPassed st >= stMaxTests st =
      doneTesting st
  -- we discarded too many tests
  | stNumDiscarded st >= stMaxDiscardRatio st * max (stNumPassed st) (stMaxTests st) =
      giveUp st "too many discarded tests"
  -- the time bugdet is over, we check this every so often
  | (stNumPassed st + stNumDiscarded st) `mod` 100 == 0 =
      ifM (passedTimeout st) (giveUp st "timeout") (newTest runner st)
  -- reset the trace log if we havent enqueued anything interesting in a while
  -- additionally, increase the number of random mutations
  | maybe False (stLastInteresting st >=) (stResetAfter st) &&
    null (stPassedQueue st) && null (stDiscardedQueue st) = do
      resetTraceLog (stPassedTraceLog st)
      resetTraceLog (stDiscardedTraceLog st)
      let st' = st { stResetAfter = maybe (stResetAfter st) (Just . (*2)) (stResetAfter st)
                   , stRandomMutations = min 256 (stRandomMutations st * 2)
                   , stTraceLogResets = stTraceLogResets st + 1
                   , stLastInteresting = 0
                   }
      newTest runner st'
  -- nothing new under the sun, continue testing
  | otherwise =
      newTest runner st

-- | Testing is no more
doneTesting :: TraceLogger log => State log -> IO Report
doneTesting st = do
  clear
  printf "Done testing\n"
  reportFinalStats st
  return AllPassed
    { numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    }

-- | Too many discarded tests
giveUp :: TraceLogger log => State log -> String -> IO Report
giveUp st r = do
  clear
  printf "Gave up (%s)\n" r
  reportFinalStats st
  return GaveUp
    { why = r
    , numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    }

-- | Found a bug!
counterexample :: TraceLogger log => State log -> Args -> Test -> IO Report
counterexample st as test = do
  clear
  reportCounterexample as test
  reportFinalStats st
  return Counterexample
    { numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    , failingArgs = as
    , failingSeed = stUsedSeed st
    }

-- | Generate and run a new test
newTest :: TraceLogger log => TestCaseRunner log -> State log -> IO Report
newTest runner st = do
  -- when we are in deep debug mode stop until the user presses enter
  when (stStepByStep st) (void getLine)
  -- print the header
  clear >> hFlush stdout
  printGlobalStats st
  -- pick a new test case
  (args, parent, st') <- pickNextTestCase st
  -- run the test case
  runRes <- runner st' parent args
  -- check the test result and continue or report a counterexample
  either (counterexample st' args)
         (loop runner)
         runRes

----------------------------------------
-- | Test case runners

-- | Run the test and check the result, if it passes then continue testing
runTestCase_tree :: TestCaseRunner TraceTreeLog
runTestCase_tree st parent args = do
  -- run the test
  when (stDebug st) $ do
    printf "\nRunning test...\n"
  -- reset the evaluated position reference
#ifdef MUTAGEN_NO_LAZY
  (test, Trace entries) <- withTrace (unResult (protectResult (stArgsRunner st args)))
  evalPos <- return Nothing
#else
  resetPosRef
  (test, Trace entries) <- withTrace (unResult (protectResult (stArgsRunner st (lazy args))))
  evalPos <- Just <$> readPosRef
  when (stDebug st) $ do
    printf "\nEvaluated subexpressions:\n%s\n" (show evalPos)
#endif
  -- record the test trace and check if it was interesting
  let tr = Trace (take (stMaxTraceLength st) entries)
  -- inspect the test result
  case test of
    -- boom!
    Failed -> do
      return (Left test)
    -- test passed, lotta work to do now
    Passed -> do
      when (stDebug st) (printf "\nTest result: PASSED\n")
      (new, depth) <- registerTrace tr (stPassedTraceLog st)
      let interesting = new > 0
      when (stDebug st) $ do
        printMutatedTestCaseTrace tr
      when interesting $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new depth
      let mbatch = createOrInheritMutationBatch st args parent evalPos True
      let st' = st { stNumPassed = stNumPassed st + 1
                   , stPassedQueue =
                       if interesting
#ifdef MUTAGEN_NO_FIFO
                       then PQueue.insertBehind 1 (args, tr, mbatch) (stPassedQueue st)
#else
                       then PQueue.insert depth (args, tr, mbatch) (stPassedQueue st)
#endif
                       else stPassedQueue st
                   , stLastInteresting =
                       if interesting
                       then 0
                       else stLastInteresting st + 1
                   }
      return (Right st')
    -- test discarded, lotta work to do here too
    Discarded -> do
      when (stDebug st) (printf "\nTest result: DISCARDED\n")
      (new, depth) <- registerTrace tr (stDiscardedTraceLog st)
      let interesting = new > 0 && maybe False mb_test_passed parent
      when (stDebug st) $ do
        printMutatedTestCaseTrace tr
      when interesting $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new depth
      let mbatch = createOrInheritMutationBatch st args parent evalPos False
      let st' = st { stNumDiscarded = stNumDiscarded st + 1
                   , stDiscardedQueue =
                       if interesting
#ifdef MUTAGEN_NO_FIFO
                       then PQueue.insertBehind 1 (args, tr, mbatch) (stDiscardedQueue st)
#else
                       then PQueue.insert depth (args, tr, mbatch) (stDiscardedQueue st)
#endif
                       else stDiscardedQueue st
                   , stLastInteresting =
                       if interesting
                       then 0
                       else stLastInteresting st + 1
                   }
      return (Right st')

-- | Run the test and check the result, if it passes then continue testing
runTestCase_bitmap :: TestCaseRunner TraceBitmapLog
runTestCase_bitmap st parent args = do
  undefined

----------------------------------------
-- Selecting the next test case

-- | Select a new test, mutating and existing interesting one or generating a
-- brand new otherwise.
pickNextTestCase :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
pickNextTestCase st
  -- we can run an example provided by the user
  | not (null (stExamples st)) = useExampleTest st
  -- we can run a mutation of an interesting succesful test case
  | not (null (stPassedQueue st)) = mutateFromPassed st
  -- we can run a mutation of an interesting discarded test case
  | not (null (stDiscardedQueue st)) = mutateFromDiscarded st
  -- only choice left is to generate a brand new test
  | otherwise = generateNewTest st

useExampleTest :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
useExampleTest st = do
  let (args:rest) = stExamples st
  printExampleTestCase args
  let (ne, ng, nmp, nmd) = stNumExGenMut st
  let st' = st { stExamples = rest
               , stNumExGenMut = (ne+1, ng, nmp, nmd)
               }
  return (args, Nothing, st')

generateNewTest :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
generateNewTest st = do
  -- first we compute an appropriate generation size
  let size = computeSize st
  -- then we randomly generate a lazily evaluated test
  let (rnd1, rnd2) = split (stNextSeed st)
  let args = unGen (stArgsGen st) rnd1 size
  printGeneratedTestCase args
  -- put the new random seed in the state
  let (ne, ng, nmp, nmd) = stNumExGenMut st
  let st' = st { stUsedSeed = Just (rnd1, size)
               , stNextSeed = rnd2
               , stNumExGenMut = (ne, ng+1, nmp, nmd)
               }
  return (args, Nothing, st')

mutateFromPassed :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
mutateFromPassed st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stPassedQueue st)
  next <- nextMutation mbatch
  case next of
    Nothing -> do
      let st' = st { stPassedQueue = rest }
      pickNextTestCase st'
    Just (args', mbatch') -> do
      when (stDebug st) $ do
        printOriginalTestCase prio args True
        printBatchStatus mbatch
        printOriginalTestCaseTrace tr
      printMutatedTestCase args'
      let (ne, ng, nmp, nmd) = stNumExGenMut st
      let st' = st { stPassedQueue = PQueue.insert prio (args, tr, mbatch') rest
                   , stNumExGenMut = (ne, ng, nmp+1, nmd)
                   }
      return (args', Just mbatch', st')

mutateFromDiscarded :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
mutateFromDiscarded st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stDiscardedQueue st)
  next <- nextMutation mbatch
  case next of
    Nothing -> do
      let st' = st { stDiscardedQueue = rest }
      pickNextTestCase st'
    Just (args', mbatch') -> do
      when (stDebug st) $ do
        printOriginalTestCase prio args False
        printBatchStatus mbatch
        printOriginalTestCaseTrace tr
      printMutatedTestCase args'
      let (ne, ng, nmp, nmd) = stNumExGenMut st
      let st' = st { stDiscardedQueue = PQueue.insert prio (args, tr, mbatch') rest
                   , stNumExGenMut = (ne, ng, nmp, nmd+1)
                   }
      return (args', Just mbatch', st')
