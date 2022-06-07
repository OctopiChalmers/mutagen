{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RE.Types where

import Data.String

import Test.Mutagen
import qualified Test.Mutagen.TH as TH

----------------------------------------
-- Regular expressions

data RE a =
    Nil
  | Eps
  | Atom a
  | Star (RE a)
  | Plus (RE a) (RE a)
  | Seq  (RE a) (RE a)
  deriving (Show, Eq, Ord)

TH.deriveAll ''RE

----------------------------------------
-- ASCII characters: we don't want to use unicode

newtype ASCII = ASCII Char
  deriving (Show, Eq, Ord)

instance {-# OVERLAPS #-} IsString [ASCII] where
  fromString = fmap ASCII

----------------------------------------
-- Boilerplate

-- ASCII are a bit special: we want it to behave just as Char.

instance Arbitrary ASCII where
  arbitrary = ASCII <$> elements ['\x20' .. '\x7E']

instance Mutable ASCII where
  def = ASCII def
  inside []  mut = mut
  inside pos _   = error $ "inside: invalid position: " <> show pos
  mutate = const [ Rand arbitrary ]

instance Lazy ASCII where
  lazyNode pre (ASCII c) = ASCII (lazyNode pre c)

instance Fragmentable ASCII
