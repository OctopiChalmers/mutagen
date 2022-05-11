{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Mutagen

import RE.Types
import qualified RE.Spec  as Spec

main :: IO ()
main = do
  mutagenWith 
    defaultConfig { 
      -- Max generation size
      maxGenSize = 5,
      
      -- Go step by step
      -- debug = True,

      -- The tracing backend, either Tree or Bitmap
      traceMethod = Tree,       -- Prefix trees (Tries)
      -- traceMethod = Bitmap,  -- Edge-based bitmaps (like AFL)
      
      -- We can provide examples to initialize the fragment store
      examples = [ 
        example (Star (Atom (ASCII 'Y'))), 
        example (Plus Eps (Atom (ASCII 'X'))) 
      ], 

      -- Only store fragments of the following types
      -- (default is all types) 
      filterFragments = Just [ 
        allow @(RE ASCII),
        allow @ASCII
      ]
    }
    Spec.prop_optimize 

