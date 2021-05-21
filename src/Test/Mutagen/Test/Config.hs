{-# LANGUAGE RankNTypes #-}
module Test.Mutagen.Test.Config where

import Test.QuickCheck.Random (QCGen)

import Test.Mutagen.Mutation
import Test.Mutagen.Property

----------------------------------------
-- | Testing options (available to the user)

data TraceMethod = Tree | Bitmap

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
  , traceMethod     :: TraceMethod
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
  , traceMethod     = Tree
  }
