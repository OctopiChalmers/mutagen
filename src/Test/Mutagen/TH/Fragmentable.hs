{-# LANGUAGE TemplateHaskellQuotes #-}
module Test.Mutagen.TH.Fragmentable where

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

import Test.Mutagen.Fragment (Fragmentable, fragmentize, singleton)

import Test.Mutagen.TH.Util

----------------------------------------
-- Derive a Lazy instance for a given data type

deriveFragmentable :: Name -> [Name] -> Q [DDec]
deriveFragmentable name ignored = do
  mutagenLog $ "deriving Fragmentable instance for " <> dump name
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef name
  -- Apply the context type variables to the type name to get a target * kinded type
  let dty = applyTVs name dtvbs
  -- Keep only the constructors that are not ignored
  let wantedcons = filter (\c -> dConName c `notElem` ignored) dcons
  -- Derive lazyNode for each constructor separately
  insClause <- deriveFragmentize wantedcons
  -- Build the Mutable instance
  let insTy = DConT ''Fragmentable `DAppT` dty
  let insCxt = [ DConT ''Fragmentable `DAppT` DVarT (dTyVarBndrName tvb)
               | tvb <- dtvbs ]
  let insBody = [ DLetDec (DFunD 'fragmentize [insClause]) ]
  return [ DInstanceD Nothing Nothing insCxt insTy insBody ]


-- This one is a bit tricky, the TH desugarer removes as (@) patterns, so the
-- only way to have a variable binding the full input is to introduce it as a
-- variable and then perform a case statement to find the actual constructor.
-- Reconstructing the input using the LHS pattern doesn't work because GHC
-- cannot guarantee its type to be the same as the one being matched against.
-- One can solve this also using TypeApplications and ScopedTypeVariables, but
-- it is unnecessary and we want to avoid needing extra extensions as much as
-- possible.
deriveFragmentize :: [DCon] -> Q DClause
deriveFragmentize cons = do
  input <- newName "input"
  let inputFragment = DVarE 'singleton `DAppE` DVarE input
  let mappendExp x y = DVarE '(<>) `DAppE` x `DAppE` y
  caseCons <- forM cons $ \con -> do
    (pvs, dpat) <- createDPat con
    let fragmentizeFieldExps = [ DVarE 'fragmentize `DAppE`
                                 DVarE pv
                               | pv <- pvs ]
    let caseBody = foldl mappendExp inputFragment fragmentizeFieldExps
    return (DMatch dpat caseBody)
  let clauseBody = DCaseE (DVarE input) caseCons
  return (DClause [DVarP input] clauseBody)
