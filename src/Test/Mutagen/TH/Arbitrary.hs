{-# LANGUAGE TemplateHaskell #-}
module Test.Mutagen.TH.Arbitrary where

import Data.List

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

import Test.Mutagen (Gen, Arbitrary, Arbitrary1, Arbitrary2, arbitrary, liftArbitrary, liftArbitrary2, sized, oneof)

import Test.Mutagen.TH.Util

----------------------------------------
-- Derive an Arbitrary instance for a given data type

deriveArbitrary :: Name -> [Name] -> Q [DDec]
deriveArbitrary name ignored = do
  mutagenLog $ "deriving Arbitrary instance for " <> dump name
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef name
  -- Apply the context type variables to the type name to get a target * kinded type
  let dty = applyTVs name dtvbs
  -- Keep only the constructors that are not ignored
  let wantedcons = filter (\c -> dConName c `notElem` ignored) dcons
  -- Split terminal and recursive constructors
  let (recs, terms) = partition (\c -> any (== dty) (dConFieldsTypes (dConFields c))) wantedcons
  -- Fresh TH variables
  gen <- newName "gen"
  size <- newName "size"
  -- Create generator expressions for each constructor
  recGens <- mapM (mkConGen dty gen size) recs
  termGens <- mapM (mkConGen dty gen size) terms
  -- Build the Arbitrary instance
  let insTy = DConT ''Arbitrary `DAppT` dty
  let insCxt = [ DConT ''Arbitrary `DAppT` DVarT (dTyVarBndrName tvb)
               | tvb <- dtvbs ]
  let insGen = DLetE
               [ DFunD gen
                 [ DClause [ DVarP size ]
                   (DCaseE (DVarE size)
                    [ DMatch (DLitP (IntegerL 0))
                      (DVarE 'oneof `DAppE` mkListDExp termGens)
                    , DMatch DWildP
                      (DVarE 'oneof `DAppE` mkListDExp (termGens <> recGens))
                    ])]
               ] (DVarE 'sized `DAppE` DVarE gen)
  let insBody = [ DLetDec (DFunD 'arbitrary [ DClause [] insGen ]) ]
  return [ DInstanceD Nothing Nothing insCxt insTy insBody ]

-- | Create the appropriate generator of a constructor
mkConGen :: DType -> Name -> Name -> DCon -> Q DExp
mkConGen dty gen size (DCon _ _ cname cfields _) = do
  fieldGens <- mapM (mkConFieldGen dty gen size) (dConFieldsTypes cfields)
  return (mkApplicativeDExp cname fieldGens)

-- | Create the appropriate generator for a constructor field based
mkConFieldGen :: DType -> Name -> Name -> DType -> Q DExp
mkConFieldGen dty gen_ size_ fieldTy = dsExp =<< mkGen fieldTy
  where
    mkGen ty
      -- The field is self recursive:
      -- We use the reference to the recursive generator
      | ty .==. dty =
          [e| $(varE gen_) (max 0 ($(varE size_) - 1)) |]
      -- Some special cases in between
      | Just a <- isMaybeOf ty =
          [e| sizedMaybe $(mkGen a) |]
      -- Types of kind `* -> *`
      | (DConT f `DAppT` a) <- ty =
          ifM (isInstance ''Arbitrary1 [ConT f])
              [e| liftArbitrary $(mkGen a) |]
              [e| arbitrary |]
      -- Types of kind `* -> * -> *`
      | (DConT f `DAppT` t1 `DAppT` t2) <- ty =
          ifM (isInstance ''Arbitrary2 [ConT f])
              [e| liftArbitrary2 $(mkGen t1) $(mkGen t2) |]
              [e| arbitrary |]
      -- The field type is something else:
      -- Hope for it having an Arbitrary instance
      | otherwise =
          [e| arbitrary |]

sizedMaybe :: Gen a -> Gen (Maybe a)
sizedMaybe gen = sized $ \size ->
  if size == 0
  then return Nothing
  else liftArbitrary gen
