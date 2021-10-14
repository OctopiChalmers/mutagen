{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Mutagen.Property where

import System.Timeout
import Unsafe.Coerce

import Data.Typeable

import Test.QuickCheck (Gen, Arbitrary, arbitrary)

import Test.Mutagen.Mutation
import Test.Mutagen.Exception
import Test.Mutagen.Lazy
import Test.Mutagen.Fragment

----------------------------------------
-- Test arguments hidden behind an existential

#ifdef MUTAGEN_NO_LAZY
type IsArgs a = (Show a, Eq a, Ord a, Typeable a, Arbitrary a, Fragmentable a, Mutable a)
#else
type IsArgs a = (Show a, Eq a, Ord a, Typeable a, Arbitrary a, Fragmentable a, Mutable a, Lazy a)
#endif

data Args = forall a . IsArgs a => Args a

instance Show Args where
  show (Args arg) = show arg

instance Mutable Args where
  mutate (Args a) = fmap Args <$> mutate a

  inside pos mut (Args a) =
    fmap Args <$> inside pos mut a

  positions (Args a) = positions a

#ifndef MUTAGEN_NO_LAZY
instance Lazy Args where
  lazy (Args a) = Args (lazy a)
  lazyNode pre (Args a) = Args (lazyNode pre a)
#endif

instance Eq Args where
  Args a == Args b =
    case cast b of
      Nothing -> False
      Just b' -> a == b'

instance Ord Args where
  compare (Args a) (Args b) =
    case cast b of
      Just b' -> compare a b'
      Nothing -> LT
      -- Like for the fragments, this shouldn't be needed because the Args
      -- should be of the same type at this point. I hope it works!!

instance Fragmentable Args where
  fragmentize (Args a) = fragmentize a

----------------------------------------
-- Tests

data Test = Test
  { ok     :: Maybe Bool
  , exc    :: Maybe AnException
  , reason :: Maybe String
  } deriving Show

-- Just True  => Passed
-- Just False => Failed
-- Nothing    => Discarded

{-# COMPLETE Passed, Failed, Discarded :: Test #-}
pattern Passed, Failed, Discarded :: Test
pattern Passed    <- Test { ok = Just True }
pattern Failed    <- Test { ok = Just False }
pattern Discarded <- Test { ok = Nothing }

-- | Test constructors

bool :: Bool -> Test
bool b = Test (Just b) Nothing Nothing

passed :: Test
passed = bool True

failed :: Test
failed = bool False

discarded :: Test
discarded = Test Nothing Nothing Nothing

-- | Protecting results against certain exceptions
protectResult :: Result -> Result
protectResult (Result io) = Result $ do
  let force t = ok t == Just False `seq` t
  res <- tryEvaluateIO (fmap force io)
  case res of
    Left  e -> return (exception e)
    Right r -> return r

exception :: AnException -> Test
exception e
  | isDiscard e = discarded { exc = Just e, reason = Just "evaluated 'discard'" }
  | otherwise   = failed    { exc = Just e, reason = Just "exception" }


----------------------------------------
-- Results are computations returning tests

newtype Result = Result { unResult :: IO Test }

-- | Types that can produce results
class Res a where
  result :: a -> Result

instance Res Result where
  result = id

instance Res Test where
  result t = Result (return t)

instance Res Bool where
  result b = Result (return (bool b))

instance Res a => Res (IO a) where
  result ior = Result $ do
    r <- ior
    unResult (result r)

-- | Properties with preconditions
(==>) :: Res a => Bool -> a -> Result
(==>) True  post = result post
(==>) False _    = result discarded

infixr 2 ==>

-- | Discard a property if it takes more than some milliseconds
discardAfter :: Res a => Int -> a -> Result
discardAfter millis a = Result $ do
  let iot = unResult (result a)
  mbt <- timeout (millis * 1000) iot
  case mbt of
    Nothing -> discard
    Just t  -> return t

----------------------------------------
-- Properties as generators of arguments and runner functions

data Property = Property (Gen Args) (Args -> Result)

----------------------------------------
-- Testable properties

-- | A class for testable properties
class Testable a where
  property :: a -> Property

-- | Properties are trivially testable
instance Testable Property where
  property p = p

-- | Testable properties with one argument
instance (IsArgs a, Res b) => Testable (a -> b) where
  property f = forAll arbitrary f

forAll :: (IsArgs a, Res b) => Gen a -> (a -> b) -> Property
forAll gen f =
  Property (Args <$> gen) (\(Args as) -> result (f (unsafeCoerce as)))
