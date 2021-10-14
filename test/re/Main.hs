module Main where

import Test.Mutagen

import qualified RE.Spec  as Spec
import qualified RE.Types as Types

main :: IO ()
main = do
  mutagenWith 
    defaultConfig { 
      maxGenSize = 5,
      -- chatty = True,
      -- stepByStep = True,
      traceMethod = Bitmap,
      -- useLazyPrunning = False, 
      examples = [ Args ( Types.Star (Types.Atom (Types.ASCII '@')) ) ]
      }
    Spec.prop_optimize 

