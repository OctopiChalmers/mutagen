{-# LANGUAGE TemplateHaskell #-}
module Test.Mutagen.TH.Lazy where

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

import Test.Mutagen (Lazy, __evaluated__, lazyNode)

import Test.Mutagen.TH.Util

----------------------------------------
-- Derive a Lazy instance for a given data type

deriveLazy :: Name -> [Name] -> Q [DDec]
deriveLazy name ignored = do
  mutagenLog $ "deriving Lazy instance for " <> dump name
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef name
  -- Apply the context type variables to the type name to get a target * kinded type
  let dty = applyTVs name dtvbs
  -- Keep only the constructors that are not ignored
  let wantedcons = filter (\c -> dConName c `notElem` ignored) dcons
  -- Derive lazyNode for each constructor separately
  insClauses <- mapM deriveLazyNode wantedcons
  -- Build the Mutable instance
  let insTy = DConT ''Lazy `DAppT` dty
  let insCxt = [ DConT ''Lazy `DAppT` DVarT (dTyVarBndrName tvb)
               | tvb <- dtvbs ]
  let insBody = [ DLetDec (DFunD 'lazyNode insClauses) ]
  return [ DInstanceD Nothing Nothing insCxt insTy insBody ]


deriveLazyNode :: DCon -> Q DClause
deriveLazyNode con = do
  acc <- newName "acc"
  (pvs, condpat) <- createDPat con

  let lazyFieldExps = [ DVarE 'lazyNode `DAppE`
                        (DConE '(:) `DAppE` DLitE (IntegerL idx) `DAppE` DVarE acc) `DAppE`
                        DVarE pv
                      | (idx, pv) <- zip [0..] pvs ]
  let clauseBody = DVarE '__evaluated__ `DAppE`
                   DVarE acc `DAppE`
                   mkConDExp (dConName con) lazyFieldExps
  return (DClause [DVarP acc, condpat] clauseBody)
