{-# LANGUAGE TypeApplications  #-}
module Main where

import Test.Mutagen

import RE.Types
import qualified RE.Spec  as Spec

main :: IO ()
main = do
  mutagenWith 
    defaultConfig { 
      maxGenSize = 5,
      -- chatty = True,
      -- debug = True,
      traceMethod = Bitmap,
      examples = [ 
        example (Star (Atom (ASCII '@'))) 
      ], 
      filterFragments = Just [ 
        allow @(RE ASCII)
      ]
    }
    Spec.prop_optimize 

