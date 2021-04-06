{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Test.Mutagen.Test where

import Control.Monad

import Data.Maybe
import Data.Time.Clock.POSIX

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue

import System.IO
import System.Console.ANSI
import System.Random (split)

import Text.Printf
import Text.Pretty.Simple

import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (QCGen, newQCGen)

import Tracer
import Test.Mutagen.Property
import Test.Mutagen.Mutation
import Test.Mutagen.Lazy

----------------------------------------
-- | Testing options (available to the user)

data Config
  = Config
  { maxSuccess      :: Int
  , maxDiscardRatio :: Int
  , maxSize         :: Int
  , replay          :: Maybe (QCGen, Int)
  , chatty          :: Bool
  , stepByStep      :: Bool
  , timeout         :: Maybe Integer
  , randomMutations :: Int
  , mutationLimit   :: Maybe Int
  , resetAfter      :: Maybe Int
  , mutationOrder   :: MutationOrder
  , maxTraceLength  :: Int
  , examples        :: [Args]
  }

defaultConfig :: Config
defaultConfig =
  Config
  { maxSuccess      = 1000000
  , maxDiscardRatio = 1000
  , maxSize         = 10
  , replay          = Nothing
  , chatty          = False
  , stepByStep      = False
  , timeout         = Nothing
  , randomMutations = 1
  , mutationLimit   = Nothing
  , resetAfter      = Just 1000
  , mutationOrder   = levelorder
  , maxTraceLength  = 100
  , examples        = []
  }

----------------------------------------
-- | Testing state (internal)

data State
  = State
  -- Static
  { stMaxTests           :: Int
  , stMaxDiscardRatio    :: Int
  , stMaxSize            :: Int
  , stArgsGen            :: Gen Args
  , stArgsRunner         :: Args -> Result
  , stDebug              :: Bool
  , stStepByStep         :: Bool
  , stTimeout            :: (Maybe Integer)
  , stStartTime          :: Integer
  , stRandomMutations    :: Int
  , stMutationLimit      :: Int
  , stResetAfter         :: Maybe Int
  , stMutationOrder      :: MutationOrder
  , stMaxTraceLength     :: Int
  , stExamples           :: [Args]
  -- Dynamic
  , stUsedSeed          :: !(Maybe (QCGen, Int))
  , stNextSeed          :: !QCGen
  , stNumExGenMut       :: !(Int, Int, Int, Int)
  , stLastInteresting   :: !Int
  , stTraceLogResets    :: !Int
  , stNumPassed         :: !Int
  , stPassedTraceLog    :: !TraceLog
  , stPassedQueue       :: !MutationQueue
  , stNumDiscarded      :: !Int
  , stDiscardedTraceLog :: !TraceLog
  , stDiscardedQueue    :: !MutationQueue
  }

-- Mutation candidates
type MutationQueue =
  MinPQueue
    Int                  -- The candidate priority
    ( Args               -- The test case inputs
    , Trace              -- The code coverage it triggered
    , MutationBatch Args -- The batch of possible mutations
    )

----------------------------------------
-- | Testing results

data Report =
    AllPassed
    { numPassed    :: Int
    , numDiscarded :: Int
    }
  | Counterexample
    { numPassed    :: Int
    , numDiscarded :: Int
    , failingArgs  :: Args
    , failingSeed  :: Maybe (QCGen, Int)
    }
  | GaveUp
    { why          :: String
    , numPassed    :: Int
    , numDiscarded :: Int
    }
  deriving Show

isSuccess :: Report -> Bool
isSuccess (AllPassed {}) = True
isSuccess _           = False

isFailure :: Report -> Bool
isFailure (Counterexample {}) = True
isFailure _           = False

----------------------------------------
-- | Test drivers, mirrored from QuickCheck ones

mutagen :: Testable p => p -> IO ()
mutagen = mutagenWith defaultConfig

mutagenVerbose :: Testable p => p -> IO ()
mutagenVerbose = mutagenWith defaultConfig { chatty = True }

mutagenVerboseResult :: Testable p => p -> IO Report
mutagenVerboseResult = mutagenWithResult defaultConfig { chatty = True }

mutagenResult :: Testable p  => p -> IO Report
mutagenResult = mutagenWithResult defaultConfig

mutagenWith :: Testable p => Config -> p -> IO ()
mutagenWith cfg p = mutagenWithResult cfg p >> return ()

-- The main driver
mutagenWithResult :: Testable p => Config -> p -> IO Report
mutagenWithResult cfg p
  -- we want to replay a counterexample
  | Just (rng, size) <- replay cfg = do
      replayCounterexample (property p) (rng, size)
  -- this is not a drill!
  | otherwise = do
      -- rng generator
      rng <- newQCGen
      -- start timestamp
      now <- round <$> getPOSIXTime
      -- extract the args gen and runner from the property
      let Property gen runner = property p
      -- the initial internal state
      let st = State
            { stMaxTests = maxSuccess cfg
            , stMaxDiscardRatio = maxDiscardRatio cfg
            , stMaxSize = maxSize cfg
            , stArgsGen = gen
            , stArgsRunner = runner
            , stNextSeed = rng
            , stUsedSeed = Nothing
            , stDebug = chatty cfg
            , stStepByStep = stepByStep cfg
            , stTimeout = timeout cfg
            , stStartTime = now
            , stMaxTraceLength = maxTraceLength cfg
            , stExamples = examples cfg
            , stRandomMutations = randomMutations cfg
            , stMutationLimit = maybe (maxSize cfg) id (mutationLimit cfg)
            , stResetAfter = resetAfter cfg
            , stMutationOrder = mutationOrder cfg
            , stNumExGenMut = (0,0,0,0)
            , stLastInteresting = 0
            , stTraceLogResets = 0
            , stNumPassed = 0
            , stPassedTraceLog = emptyTraceLog
            , stPassedQueue = mempty
            , stNumDiscarded = 0
            , stDiscardedTraceLog = emptyTraceLog
            , stDiscardedQueue = mempty
            }
      -- go go go!
      loop st


----------------------------------------
-- | The main test loop

loop :: State -> IO Report
loop st
  -- we reached the max number of tests
  | stNumPassed st >= stMaxTests st =
      doneTesting st
  -- we discarded too many tests
  | stNumDiscarded st >= stMaxDiscardRatio st * max (stNumPassed st) (stMaxTests st) =
      giveUp st "too many discarded tests"
  -- the time bugdet is over, we check this every so often
  | (stNumPassed st + stNumDiscarded st) `mod` 100 == 0 =
      ifM (passedTimeout st) (giveUp st "timeout") (newTest st)
  -- reset the trace log if we havent enqueued anything interesting in a while
  -- additionally, increase the number of random mutations
  | maybe False (stLastInteresting st >=) (stResetAfter st) &&
    null (stPassedQueue st) && null (stDiscardedQueue st) =
      let st' = st { stPassedTraceLog = emptyTraceLog
                   , stDiscardedTraceLog = emptyTraceLog
                   , stResetAfter = maybe (stResetAfter st) (Just . (*2)) (stResetAfter st)
                   , stRandomMutations = min 256 (stRandomMutations st * 2)
                   , stTraceLogResets = stTraceLogResets st + 1
                   , stLastInteresting = 0
                   }
      in newTest st'
  -- nothing new under the sun, continue testing
  | otherwise =
      newTest st

-- | Too many discarded tests
giveUp :: State -> String -> IO Report
giveUp st r = do
  clear
  printf "Gave up (%s)\n" r
  reportFinalStats st
  return GaveUp
    { why = r
    , numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    }

-- | Testing is no more
doneTesting :: State -> IO Report
doneTesting st = do
  clear
  printf "Done testing\n"
  reportFinalStats st
  return AllPassed
    { numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    }

-- | Found a bug!
counterexample :: State -> Args -> Test -> IO Report
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
newTest :: State -> IO Report
newTest st = do
  when (stStepByStep st) (void getLine)
  -- void getLine
  clear >> hFlush stdout
  printGlobalStats st
  -- pick a new test case
  (args, mbatch, st') <- pickNewTest st
  -- check the result and continue
  runTestCase st' args mbatch

-- | Select a new test, mutating and existing interesting one or generating a
-- brand new otherwise.
pickNewTest :: State -> IO (Args, Maybe (MutationBatch Args), State)
pickNewTest st
  -- we can run an example provided by the user
  | not (null (stExamples st)) = useExampleTest st
  -- we can run a mutation of an interesting succesful test case
  | not (null (stPassedQueue st)) = mutateFromPassed st
  -- we can run a mutation of an interesting discarded test case
  | not (null (stDiscardedQueue st)) = mutateFromDiscarded st
  -- only choice left is to generate a brand new test
  | otherwise = generateNewTest st

useExampleTest :: State -> IO (Args, Maybe (MutationBatch Args), State)
useExampleTest st = do
  let (args:rest) = stExamples st
  printExampleTestCase args
  let (ne, ng, nmp, nmd) = stNumExGenMut st
  let st' = st { stExamples = rest
               , stNumExGenMut = (ne+1, ng, nmp, nmd)
               }
  return (args, Nothing, st')

generateNewTest :: State -> IO (Args, Maybe (MutationBatch Args), State)
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

mutateFromPassed :: State -> IO (Args, Maybe (MutationBatch Args), State)
mutateFromPassed st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stPassedQueue st)
  next <- nextMutation mbatch
  case next of
    Nothing -> do
      let st' = st { stPassedQueue = rest }
      pickNewTest st'
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

mutateFromDiscarded :: State -> IO (Args, Maybe (MutationBatch Args), State)
mutateFromDiscarded st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stDiscardedQueue st)
  next <- nextMutation mbatch
  case next of
    Nothing -> do
      let st' = st { stDiscardedQueue = rest }
      pickNewTest st'
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


-- | Inherit the mutation batch of a parent test case of it exists, otherwise create a new one.
createOrInheritMutationBatch :: State -> Args -> Maybe (MutationBatch Args) -> Bool -> [Pos] -> MutationBatch Args
createOrInheritMutationBatch st args parentbatch isPassed pos =
  case parentbatch of
    -- test case was mutated from an existing one, we can augment its parent mutation batch
    Just mb ->
      newMutationBatchFromParent mb
        isPassed pos args
    -- test case was freshly generated, we need to initialize a new mutation batch
    Nothing ->
      newMutationBatch (stMutationOrder st) (stRandomMutations st) (stMaxSize st) (stMutationLimit st)
        isPassed pos args


-- | Run the test and check the result, if it passes then continue testing
runTestCase :: State -> Args -> Maybe (MutationBatch Args) -> IO Report
runTestCase st args parentbatch = do
  -- run the test
  when (stDebug st) $ do
    printf "\nRunning test...\n"
  -- reset the evaluated position reference
#ifdef MUTAGEN_NO_LAZY
  (test, Trace entries) <- withTrace (unResult (protectResult (stArgsRunner st args)))
  pos <- return []
#else
  resetPosRef
  (test, Trace entries) <- withTrace (unResult (protectResult (stArgsRunner st (lazy args))))
  pos <- readPosRef
  when (stDebug st) $ do
    printf "\nEvaluated subexpressions:\n%s\n" (show pos)
#endif
  -- record the test trace and check if it was interesting
  let tr = Trace (take (stMaxTraceLength st) entries)
  -- inspect the test result
  case test of
    Failed -> do
      counterexample st args test
    Passed -> do
      when (stDebug st) (printf "\nTest result: PASSED\n")
      let (tlog', new, depth) = registerTrace tr (stPassedTraceLog st)
      let interesting = new > 0
      when (stDebug st) $ do
        printMutatedTestCaseTrace tr
      when interesting $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new depth
      let mbatch = createOrInheritMutationBatch st args parentbatch True pos
      loop st { stNumPassed = stNumPassed st + 1
              , stPassedTraceLog = tlog'
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
    Discarded -> do
      when (stDebug st) (printf "\nTest result: DISCARDED\n")
      let (tlog', new, depth) = registerTrace tr (stDiscardedTraceLog st)
      let interesting = new > 0 && parentPassed parentbatch
      when (stDebug st) $ do
        printMutatedTestCaseTrace tr
      when interesting $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new depth
      let mbatch = createOrInheritMutationBatch st args parentbatch False pos
      loop st { stNumDiscarded = stNumDiscarded st + 1
              , stDiscardedTraceLog = tlog'
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


-- | Check whether the timeout has passed
passedTimeout :: State -> IO Bool
passedTimeout st
  | Just s <- stTimeout st = do
      now <- round <$> getPOSIXTime
      return (now >= stStartTime st + s)
  | otherwise = return False


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
-- Terminal reporters

printBatchStatus :: MutationBatch Args -> IO ()
printBatchStatus mbatch = do
  printf "Current mutation batch: %d tests enqueued, %d mutations left\n"
    (length (mb_curr_queue mbatch)) (mb_nmuts mbatch)
  printf "\nMutated positions:\n"
  mapM_ (\pos -> putStrLn (show pos <> " *")) (reverse (mb_past_pos mbatch))
  printf "\nNext mutable positions:\n"
  case mb_next_pos mbatch of
    [] -> return ()
    (p:ps) -> do
      putStrLn (show p <> " <== current")
      mapM_ print ps

printExampleTestCase :: Args -> IO ()
printExampleTestCase args = do
  printf "\nUsing provided example test case:\n"
  prettyPrint args

printGeneratedTestCase :: Args -> IO ()
printGeneratedTestCase args = do
  printf "\nGenerated new random test case:\n"
  prettyPrint args

printOriginalTestCase :: Int -> Args -> Bool -> IO ()
printOriginalTestCase prio args isPassed = do
  printf "\nMutating %s test case (prio=%d):\n"
    (if isPassed then "passed" else "discarded") prio
  prettyPrint args

printMutatedTestCase :: Args -> IO ()
printMutatedTestCase args = do
  printf "\nMutated test case:\n"
  prettyPrint args

printOriginalTestCaseTrace :: Trace -> IO ()
printOriginalTestCaseTrace tr = do
  printf "\nOriginal trace:\n"
  putStrLn (show (unTrace tr))

printMutatedTestCaseTrace :: Trace -> IO ()
printMutatedTestCaseTrace tr = do
  printf "\nNew trace:\n"
  putStrLn (show (unTrace tr))

printGlobalStats :: State -> IO ()
printGlobalStats st = do
  let (ne, ngen, nmp, nmd) = stNumExGenMut st
  printf "Statistics:\n"
  printf "* Passed %d tests (%d discarded)\n"
    (stNumPassed st) (stNumDiscarded st)
  printf "* Tests origin: %d examples, %d generated, %d mutated from passed, %d mutated from discarded\n"
    ne ngen nmp nmd
  printf "* Enqueued tests for mutation: %d passed, %d discarded\n"
    (PQueue.size (stPassedQueue st)) (PQueue.size (stDiscardedQueue st))
  printf "* Tests since last interesting: %d \t(trace log was reset %d times)\n"
    (stLastInteresting st) (stTraceLogResets st)

reportCounterexample :: Args -> Test -> IO ()
reportCounterexample as res = do
  printf "Found counterexample!\n"
  printf "* Reason of failure: %s\n"
    (maybe "assertion failed" id (reason res))
  when (isJust (exc res)) $ do
    printf "* The exception was:\n%s\n" (show (fromJust (exc res)))
  printf "* Failing inputs:\n"
  prettyPrint as
  printf "\n"

reportFinalStats :: State -> IO ()
reportFinalStats st = do
  printGlobalStats st
  now <- round <$> getPOSIXTime
  let elapsed = now - stStartTime st
  printf "* Elapsed time: %d seconds (+/- 1 second)\n" elapsed

put :: String -> IO ()
put str = putStr str >> hFlush stdout

clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0 >> cursorUp 1

prettyPrint :: Show a => a -> IO ()
prettyPrint a =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsPageWidth = 120
      , outputOptionsCompact = False
      , outputOptionsCompactParens = True
      }
    a

----------------------------------------
-- Helpers

parentPassed :: Maybe (MutationBatch Args) -> Bool
parentPassed (Nothing) = False
parentPassed (Just mb) = mb_test_passed mb

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM mb mth mel = mb >>= \b -> if b then mth else mel

computeSize :: State -> Int
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
