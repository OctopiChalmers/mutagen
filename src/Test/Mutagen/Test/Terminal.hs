module Test.Mutagen.Test.Terminal where

import Control.Monad

import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.PQueue.Prio.Min as PQueue

import System.IO
import System.Console.ANSI

import Text.Printf
import Text.Pretty.Simple

import Tracer.Trace
import Test.Mutagen.Property
import Test.Mutagen.Test.State
import Test.Mutagen.Test.Batch

----------------------------------------
-- Terminal reporters

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

printGlobalStats :: State log -> IO ()
printGlobalStats st = do
  let (ne, ngen, nmp, nmd) = stNumExGenMut st
  printf "Statistics:\n"
  printf "* Passed %d tests (%d discarded)\n"
    (stNumPassed st) (stNumDiscarded st)
  printf "* Tests origin: %d examples, %d generated, %d mutated from passed, %d mutated from discarded\n"
    ne ngen nmp nmd
  printf "* Enqueued tests for mutation: %d passed, %d discarded\n"
    (PQueue.size (stPassedQueue st)) (PQueue.size (stDiscardedQueue st))
  printf "* Total interesting test cases: %d\n"
    (stNumInteresting st)
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

reportFinalStats :: State log -> IO ()
reportFinalStats st = do
  printGlobalStats st
  now <- round <$> getPOSIXTime
  let elapsed = now - stStartTime st
  printf "* Elapsed time: %d seconds (+/- 1 second)\n" elapsed

----------------------------------------
-- Helpers

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
