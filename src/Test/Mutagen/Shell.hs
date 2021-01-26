module Test.Mutagen.Shell where

import Text.Printf
import Control.Monad

import System.Process
import System.Exit

-- Run a shell command passing inputs via stdin and collecting stdout
shell :: FilePath -> [String] -> String -> IO String
shell cmd args input = readProcess cmd args input

-- Run a shell command passing inputs via stdin and collecting stdout
shellExitCode :: Bool -> FilePath -> [String] -> String -> IO ExitCode
shellExitCode verbose cmd args input = do
  (e, sout, serr) <- readProcessWithExitCode cmd args input
  when verbose $ do
    printf "\n-----\nShell output:\n  stdout: %s\n  stderr: %s\n-----\n" sout serr
  return e

hasFailed :: ExitCode -> Bool
hasFailed (ExitFailure n) = n < 0 || (n > 128 && n < 143)
hasFailed ExitSuccess = False

isExitSuccess :: ExitCode -> Bool
isExitSuccess ExitSuccess = True
isExitSuccess _           = False
