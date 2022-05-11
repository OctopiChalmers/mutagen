module Test.Mutagen.Test.Report where

import Test.Mutagen.Property

----------------------------------------
-- | Testing results

data Report =
    AllPassed
    { numPassed :: Int
    , numDiscarded :: Int
    }
  | Counterexample
    { numPassed :: Int
    , numDiscarded :: Int
    , failingArgs :: Args
    }
  | GaveUp
    { why :: String
    , numPassed :: Int
    , numDiscarded :: Int
    }
  deriving Show

isSuccess :: Report -> Bool
isSuccess (AllPassed {}) = True
isSuccess _              = False

isFailure :: Report -> Bool
isFailure (Counterexample {}) = True
isFailure _                   = False
