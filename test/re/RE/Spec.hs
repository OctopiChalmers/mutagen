module RE.Spec where

import Test.Mutagen

import RE.Types
import RE.Match

prop_optimize :: (RE ASCII, [ASCII]) -> Result
prop_optimize (re, str) =
  not (null str) && re `matches` str ==>
  optimize re `matches` str
