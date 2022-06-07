{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Mutagen.Test.Config where

import Data.Typeable

import Test.Mutagen.Mutation
import Test.Mutagen.Property

----------------------------------------
-- | Testing options (available to the user)

data TraceMethod = Tree | Bitmap

data Config
  = Config
  ----------------------------------------
  -- Campaign options
  { maxSuccess :: Int
  -- ^ Max number of passed tests
  , maxDiscardRatio :: Int
  -- ^ Max discard ratio
  , timeout :: Maybe Integer
  -- ^ Campaign time budget in seconds (has precedence over maxSuccess)

  ----------------------------------------
  -- Generation options
  , maxGenSize :: Int
  -- ^ Max generation size passed to a generator. It uses the same formula for
  -- computing sizes as vanilla QuickCheck when in generation mode. Random
  -- mutations are generated using the maximum size.

  ----------------------------------------
  -- Mutation options
  , randomMutations :: Int
  -- ^ The amount of times to sample the generator associated to a random
  -- mutant. It can be automatically increased over time if `autoResetAfter` is not
  -- set to `Nothing`.
  , randomFragments :: Int
  -- ^ The amount of fragments sampled from the global fragment store when
  -- concretizing a fragment mutant. Can return less than `randomFragments` test
  -- cases if there are not enough fragments of the type of the target
  -- subexpression to sample from.
  , mutationLimit :: Maybe Int
  -- ^ The maximum number of ancestors a test case can have before being
  -- discarded. Useful to avoid mutating recursive structures indefinetely.
  -- Defaults to `maxGenSize` if set to `Nothing`.
  , autoResetAfter :: Maybe Int
  -- ^ Reset the global trace log if no interesting test case is found after a
  -- certain number of tests. If not set to `Nothing`, this will duplicate the
  -- current limit on every reset. Additionally, it also duplicates the
  -- `randomMutations` parameter.
  , useLazyPrunning :: Bool
  -- ^ Use lazy prunning to avoid mutating unevaluated subexpressions. The
  -- target mutable subexpressions are ordered by last evaluated first.
  , mutationOrder :: MutationOrder
  -- ^ If `useLazyPrunning` is set to `False`, *every* subexpression of an
  -- interesting test case is mutated regardless whether it was evaluated or
  -- not. These subexpressions are ordered using a generic tree traversal order
  -- (level order by default). Options are: `levelorder`, `preorder`, and
  -- `postorder`.
  , useFragments :: Bool
  -- ^ Explode the interesting test cases found during the test loop into typed
  -- fragments. These fragments can be used to concretize fragment mutants.
  , filterFragments :: Maybe [TypeRep]
  -- ^ If not set to `Nothing`, the loop collect and use fragments of only some
  -- specific types.
  , examples :: [Args]
  -- ^ Initial inputs examples used to populate the global fragment store before
  -- the testing loop starts.

  ----------------------------------------
  -- Tracing options
  , traceMethod :: TraceMethod
  -- ^ The tracing log mechanism. Either `Tree` or `Bitmap`. `Tree` uses
  -- prefix-based traces (quite expensive but more precise). `Bitmap` uses
  -- edge-based traces (cheaper but less precise).
  , maxTraceLength :: Maybe Int
  -- ^ The maximim trace length to consider. Useful in conjunction with the
  -- `Tree` `traceMethod` when testing lengthy properties.

  ----------------------------------------
  -- Debug options
  , chatty :: Bool
  -- ^ Print extra info
  , debug :: Bool
  -- ^ Stop after every step and wait for the user to press Enter.
  }

defaultConfig :: Config
defaultConfig =
  Config
  { maxSuccess      = 1000000
  , maxDiscardRatio = 1000
  , timeout         = Nothing
  , maxGenSize      = 10
  , randomMutations = 1
  , randomFragments = 10
  , mutationLimit   = Nothing
  , autoResetAfter  = Just 100
  , useLazyPrunning = False
  , mutationOrder   = levelorder
  , useFragments    = False
  , filterFragments = Nothing
  , examples        = []
  , traceMethod     = Tree
  , maxTraceLength  = Nothing
  , chatty          = False
  , debug           = False
  }


allow :: forall a. Typeable a => TypeRep
allow = typeRep (Proxy @a)

example :: forall a. IsArgs a => a -> Args
example = Args
