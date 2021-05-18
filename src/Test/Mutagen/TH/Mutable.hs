{-# LANGUAGE TemplateHaskell #-}
module Test.Mutagen.TH.Mutable where

import Data.List
import Data.Ord

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

import Test.Mutagen (Mutable, def, positions, node, inside, wrap, invalidPosition, mutate, Mutant(..))

import Test.Mutagen.TH.Util

----------------------------------------
-- Derive a Mutable instance for a given data type

deriveMutable :: Name -> [Name] -> Maybe Name -> Q [DDec]
deriveMutable name ignored mbdef = do
  mutagenLog $ "deriving Mutable instance for " <> dump name
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef name
  -- Apply the context type variables to the type name to get a target * kinded type
  let dty = applyTVs name dtvbs
  -- Keep only the constructors that are not ignored
  let wantedcons = filter (\c -> dConName c `notElem` ignored) dcons
  -- Derive each function in the Mutable class separately
  insDef       <- deriveDef dty mbdef wantedcons
  insPositions <- derivePositions     wantedcons
  insInside    <- deriveInside        wantedcons
  insMutate    <- deriveMutate        wantedcons
  -- Build the Mutable instance
  let insTy = DConT ''Mutable `DAppT` dty
  let insCxt = [ DConT ''Mutable `DAppT` DVarT (dTyVarBndrName tvb)
               | tvb <- dtvbs ]
  let insBody = concat [insDef, insPositions, insInside, insMutate]
  return [ DInstanceD Nothing Nothing insCxt insTy insBody ]

deriveDef :: DType -> Maybe Name -> [DCon] -> Q [DDec]
deriveDef dty mbdef cons = do
  case mbdef of
    Just var -> do
      let defValue = DVarE var
      return [ DLetDec (DFunD 'def [ DClause [] defValue ]) ]
    Nothing -> do
      let terms = filter (\c -> not (any (== dty) (dConFieldsTypes (dConFields c)))) cons
      let sorted = sortBy (comparing (dConFieldsNum . dConFields)) terms
      when (null sorted) $
        mutagenError "could not find a proper constructor to derive def automatically, please a default value manually" [sorted]
      let smallest = head sorted
      let defValue = mkConDExp (dConName smallest) (replicate (dConFieldsNum (dConFields smallest)) (DVarE 'def))
      return [ DLetDec (DFunD 'def [ DClause [] defValue ]) ]

derivePositions :: [DCon] -> Q [DDec]
derivePositions cons = do
  clauses <- forM cons $ \con -> do
    (pvs, dpat) <- createDPat con
    let clauseBody = DVarE 'node `DAppE`
                     mkListDExp [ mkTupleDExp [ DLitE (IntegerL n)
                                              , DVarE 'positions `DAppE` DVarE var ]
                                | (n, var) <- zip [0..] pvs ]
    return (DClause [dpat] clauseBody)
  return [ DLetDec (DFunD 'positions clauses) ]

deriveInside :: [DCon] -> Q [DDec]
deriveInside cons = do
  pos <- newName "pos"
  mut <- newName "mut"
  x <- newName "x"
  -- first clause
  let firstclause = DClause [DConP '[] [], DVarP mut, DVarP x] (DVarE mut `DAppE` DVarE x)
  -- recursive constructor clauses
  conclauses <- forM cons $ \con -> do
    (pvs, condpat) <- createDPat con
    forM [0 .. length pvs - 1] $ \idx -> do
      let posdpat = DConP '(:) [DLitP (IntegerL (fromIntegral idx)), DVarP pos]
      let mutdpat = DVarP mut
      let clauseBody = DVarE 'wrap `DAppE`
                       (DVarE 'inside `DAppE` DVarE pos `DAppE` DVarE mut `DAppE` DVarE (pvs !! idx)) `DAppE`
                       (DLamE [x] (mkConDExp (dConName con) ([ DVarE v | v <- replace idx pvs x ])))
      return (DClause [posdpat, mutdpat, condpat] clauseBody)
  -- last clause (error message)
  let lastclause = DClause [DVarP pos, DWildP, DWildP] (DVarE 'invalidPosition `DAppE` DVarE pos)
  return [ DLetDec (DFunD 'inside ([firstclause] <> concat conclauses <> [lastclause])) ]

deriveMutate :: [DCon] -> Q [DDec]
deriveMutate cons = do
  clauses <- forM cons $ \con -> do
    (pvs, dpat) <- createDPat con
    let pvsTys = zip pvs (dConFieldsTypes (dConFields con))
    let clauseBody = mkListDExp [ DConE 'Pure `DAppE` mc
                                | mc <- mutateCon (dConName con) pvsTys cons ]
    return (DClause [dpat] clauseBody)
  return [ DLetDec (DFunD 'mutate clauses) ]

mutateCon :: Name -> [(Name, DType)] -> [DCon] -> [DExp]
mutateCon name fvtys allCons = do
  con <- allCons
  let fieldSubsts = [ let subst = validSubst fty
                      in if null subst then [ 'def ] else subst
                    | fty <- dConFieldsTypes (dConFields con) ]
  filter (/= ogCon) (addMutatedFields (DConE (dConName con)) fieldSubsts)
  where
    ogCon = mkConDExp name [ DVarE (fst vt) | vt <- fvtys ]

    validSubst fty' = do
      (fv, fty) <- fvtys
      guard (fty' == fty)
      return fv

    addMutatedFields acc [] = do
      return acc
    addMutatedFields acc (fvs:fvss) = do
      fv <- fvs
      addMutatedFields (acc `DAppE` DVarE fv) fvss
