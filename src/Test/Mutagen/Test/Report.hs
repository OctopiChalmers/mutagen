module Test.Mutagen.Test.Report where

import Test.QuickCheck.Random (QCGen)

import Test.Mutagen.Property

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
