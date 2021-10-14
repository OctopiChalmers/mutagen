{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RE.Types where

import Data.String

import Test.Mutagen
import Test.Mutagen.TH

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

deriveInstance ''Arbitrary    ''RE
deriveInstance ''Mutable      ''RE
deriveInstance ''Lazy         ''RE
deriveInstance ''Fragmentable ''RE

----------------------------------------
-- ASCII characters: we don't want to use unicode

newtype ASCII = ASCII Char
  deriving (Show, Eq, Ord)

instance {-# OVERLAPS #-} IsString [ASCII] where
  fromString = fmap ASCII

----------------------------------------
-- Boilerplate

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
  

-- instance Arbitrary a => Arbitrary (RE a) where
--   arbitrary = sized gen
--     where
--       gen 0 =
--         oneof
--         [ pure Nil
--         , pure Eps
--         , Atom <$> arbitrary
--         ]
--       gen n =
--         frequency
--         [ (1, pure Nil)
--         , (1, pure Eps)
--         , (1, Atom <$> arbitrary)
--         , (div n 2, Star <$> gen (n-1))
--         , (n, Plus <$> gen (n-1) <*> gen (n-1))
--         , (n, Seq  <$> gen (n-1) <*> gen (n-1))
--         ]

-- instance Mutable a => Mutable (RE a) where
--   def = Eps
--   mutate Nil =
--     [ Pure Eps
--     , Pure (Atom def)
--     , Pure (Star def)
--     , Pure (Plus def def)
--     , Pure (Seq def def)
--     ]
--   mutate Eps =
--     [ Pure Nil
--     , Pure (Atom def)
--     , Pure (Star def)
--     , Pure (Plus def def)
--     , Pure (Seq def def)
--     ]
--   mutate (Atom _) =
--     [ Pure Nil
--     , Pure Eps
--     , Pure (Star def)
--     , Pure (Plus def def)
--     , Pure (Seq def def)
--     ]
--   mutate (Star x) =
--     [ Pure x
--     , Pure Nil
--     , Pure Eps
--     , Pure (Atom def)
--     , Pure (Plus x x)
--     , Pure (Seq x x)
--     ]
--   mutate (Plus x y) =
--     [ Pure x
--     , Pure y
--     , Pure Nil
--     , Pure Eps
--     , Pure (Atom def)
--     , Pure (Star x)
--     , Pure (Star y)
--     , Pure (Plus x x)
--     , Pure (Plus y y)
--     , Pure (Plus y x)
--     , Pure (Seq x x)
--     , Pure (Seq y y)
--     , Pure (Seq x y)
--     , Pure (Seq y x)
--     ]
--   mutate (Seq x y) =
--     [ Pure x
--     , Pure y
--     , Pure Nil
--     , Pure Eps
--     , Pure (Atom def)
--     , Pure (Star x)
--     , Pure (Star y)
--     , Pure (Plus x x)
--     , Pure (Plus y y)
--     , Pure (Plus x y)
--     , Pure (Plus y x)
--     , Pure (Seq x x)
--     , Pure (Seq y y)
--     , Pure (Seq y x)
--     ]

  -- inside []     mut x          = mut x
  -- inside (0:ps) mut (Atom x)   = wrap (inside ps mut x) (\x' -> Atom x')
  -- inside (0:ps) mut (Star x)   = wrap (inside ps mut x) (\x' -> Star x')
  -- inside (0:ps) mut (Plus x y) = wrap (inside ps mut x) (\x' -> Plus x' y)
  -- inside (0:ps) mut (Seq x y)  = wrap (inside ps mut x) (\x' -> Seq x' y)
  -- inside (1:ps) mut (Plus x y) = wrap (inside ps mut y) (\y' -> Plus x y')
  -- inside (1:ps) mut (Seq x y)  = wrap (inside ps mut y) (\y' -> Seq x y')
  -- inside pos    _   _          = invalidPosition pos

  -- positions Eps        = node []
  -- positions Nil        = node []
  -- positions (Atom a)   = node [ (0, positions a) ]
  -- positions (Star x)   = node [ (0, positions x) ]
  -- positions (Plus x y) = node [ (0, positions x), (1, positions y) ]
  -- positions (Seq x y)  = node [ (0, positions x), (1, positions y) ]
