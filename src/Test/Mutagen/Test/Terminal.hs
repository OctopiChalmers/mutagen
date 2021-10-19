module Test.Mutagen.Test.Terminal where

import Control.Monad

import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.PQueue.Prio.Min as PQueue

import System.IO
import System.Console.ANSI

import Text.Printf
import Text.Pretty.Simple

import Test.Mutagen.Tracer.Trace
import Test.Mutagen.Property
import Test.Mutagen.Mutation
import Test.Mutagen.Fragment
import Test.Mutagen.Test.State
import Test.Mutagen.Test.Batch

----------------------------------------
-- Terminal reporters

printDot :: IO ()
printDot = do
  printf "."
  hFlush stdout

printRunningTest :: IO ()
printRunningTest = do
  printf ">>> Running test...\n"

printEvaluatedSubexpressions :: [Pos] -> IO ()
printEvaluatedSubexpressions pos = do
  printf ">>> Evaluated subexpressions:\n"
  print pos

printTestCaseWasInteresting :: Int -> Int -> IO ()
printTestCaseWasInteresting new prio = do
  printf ">>> Test case was interesting! (new trace nodes=%d, prio=%d)\n" new prio

printTestResult :: String -> IO ()
printTestResult res = do
  printf ">>> Test result: %s\n" res

printGeneratedTestCase :: Args -> IO ()
printGeneratedTestCase args = do
  printf ">>> Generated new random test case:\n"
  prettyPrint args

printOriginalTestCase :: Int -> Args -> Bool -> IO ()
printOriginalTestCase prio args isPassed = do
  printf ">>> Mutating %s test case (prio=%d):\n"
    (if isPassed then "passed" else "discarded") prio
  prettyPrint args

printMutatedTestCase :: Args -> IO ()
printMutatedTestCase args = do
  printf ">>> Mutated test case:\n"
  prettyPrint args

printOriginalTestCaseTrace :: Trace -> IO ()
printOriginalTestCaseTrace tr = do
  printf ">>> Original trace:\n"
  putStrLn (show (unTrace tr))

printMutatedTestCaseTrace :: Trace -> IO ()
printMutatedTestCaseTrace tr = do
  printf ">>> New trace:\n"
  putStrLn (show (unTrace tr))

printBatchStatus :: MutationBatch Args -> IO ()
printBatchStatus mbatch = do
  printf ">>> Current mutation batch: %d tests enqueued, %d mutations left\n"
    (length (mb_curr_queue mbatch)) (mb_rand_num mbatch)
  printf ">>> Mutated positions:\n"
  mapM_ (\pos -> putStrLn (show pos <> " *")) (reverse (mb_past_pos mbatch))
  printf ">>> Next mutable positions:\n"
  case mb_next_pos mbatch of
    [] -> return ()
    (p:ps) -> do
      putStrLn (show p <> " <== current")
      mapM_ print ps

printGlobalStats :: State log -> IO ()
printGlobalStats st = do
  printf ">>> Statistics:\n"
  printf "* Executed test cases: %d (%d interesting, %d boring) (last interesting was %d tests ago)\n"
    (stNumInteresting st + stNumBoring st) (stNumInteresting st) (stNumBoring st) (stNumTestsSinceLastInteresting st)
  printf "* Passed %d tests (%d discarded)\n"
    (stNumPassed st) (stNumDiscarded st)
  printf "* Tests origin: %d generated, %d mutated from passed, %d mutated from discarded\n"
    (stNumGenerated st) (stNumMutatedFromPassed st) (stNumMutatedFromDiscarded st)
  printf "* Mutant kinds: %d pure, %d random, %d fragments\n"
    (stNumPureMutants st) (stNumRandMutants st) (stNumFragMutants st)
  printf "* Enqueued tests for mutation: %d passed, %d discarded\n"
    (PQueue.size (stPassedQueue st)) (PQueue.size (stDiscardedQueue st))
  printf "* Auto-reset is %s, using %d random mutations (after %d trace log resets)\n"
    (maybe "off" (const "on") (stAutoResetAfter st)) (stRandomMutations st) (stNumTraceLogResets st)
  printf "* Current generation size: %d\n"
    (stCurrentGenSize st)
  printf "* Fragment store size: %s\n"
    (show (fragmentStoreSize (stFragmentStore st)))
  now <- round <$> getPOSIXTime
  let elapsed = now - stStartTime st
  printf "* Elapsed time: %d seconds (+/- 1 second)\n" elapsed
  printf "\n"

reportDoneTesting :: State log -> IO ()
reportDoneTesting st = do
  clear
  printGlobalStats st
  printf ">>> Done testing\n"

reportGaveUp :: State log -> String -> IO ()
reportGaveUp st r = do
  clear
  printGlobalStats st
  printf ">>> Gave up (%s)\n" r

reportCounterexample :: State log -> Args -> Test -> IO ()
reportCounterexample st as res = do
  clear
  printGlobalStats st
  printf ">>> Found counterexample!\n"
  printf "* Reason of failure: %s\n"
    (maybe "assertion failed" id (reason res))
  when (isJust (exc res)) $ do
    printf "* The exception was:\n%s\n" (show (fromJust (exc res)))
  printf "* Failing inputs:\n"
  prettyPrint as
  printf "\n"

----------------------------------------
-- Helpers

put :: String -> IO ()
put str = putStr str >> hFlush stdout

clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0 >> cursorUp 1 >> hFlush stdout

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
