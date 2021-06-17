{-# LANGUAGE TemplateHaskell #-}
module Test.Mutagen.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

import Test.Mutagen

import Test.Mutagen.TH.Arbitrary
import Test.Mutagen.TH.Mutable
import Test.Mutagen.TH.Lazy
import Test.Mutagen.TH.Fragmentable
import Test.Mutagen.TH.Util

----------------------------------------
-- | Derivation options

data THOpts =
  THOpts
  { th_ignore :: [Name]  -- used for Arbitrary and Mutable
  , th_def :: Maybe Name -- used for Mutable
  }

defaultTHOpts :: THOpts
defaultTHOpts =
  THOpts
  { th_ignore = []
  , th_def = Nothing
  }

----------------------------------------
-- | TH dispatchers

deriveInstance :: Name -> Name -> Q [Dec]
deriveInstance = deriveInstanceWithOpts defaultTHOpts

deriveInstanceWithOpts :: THOpts -> Name -> Name -> Q [Dec]
deriveInstanceWithOpts opts name ty
  | name == ''Arbitrary =
    sweeten <$> deriveArbitrary ty (th_ignore opts)
  | name == ''Mutable =
    sweeten <$> deriveMutable ty (th_ignore opts) (th_def opts)
  | name == ''Lazy =
    sweeten <$> deriveLazy ty (th_ignore opts)
  | name == ''Fragmentable =
    sweeten <$> deriveFragmentable ty (th_ignore opts)
  | otherwise =
    mutagenError "type class not supported" [name]
