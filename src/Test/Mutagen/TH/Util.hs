{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Mutagen.TH.Util where

import Control.Monad
import Control.Monad.IO.Class

import System.Console.ANSI

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

----------------------------------------
-- | Reification utilities

-- | Reify a name or die gracefully
reifyName :: Name -> Q DInfo
reifyName name = do
  mbInfo <- dsReify name
  case mbInfo of
    Just info -> return info
    Nothing -> mutagenError "could not reify name" [name]

reifyTypeDef :: Name -> Q ([DTyVarBndrUnit], [DCon])
reifyTypeDef name = do
  dinfo <- reifyName name
  case dinfo of
    DTyConI (DDataD _ _ _ tvbs _ dcons _) _ -> return (tvbs, dcons)
    _ -> mutagenError ("unexpected result when reifying " <> dump name) [dinfo]

----------------------------------------
-- | Observations over DTypes

-- | Simplify a DType removing foralls and signatures
simplifyDType :: DType -> DType
simplifyDType (DForallT _ t)   = simplifyDType t
simplifyDType (DSigT t _)      = simplifyDType t
simplifyDType (DAppT l r)      = DAppT (simplifyDType l) (simplifyDType r)
simplifyDType t                = t

-- | Compare DTypes for equality
(.==.) :: DType -> DType -> Bool
t1 .==. t2 = simplifyDType t1 == simplifyDType t2

-- | Split a function type into its parameters and return type
splitSignature :: DType -> [DType]
splitSignature (DArrowT `DAppT` l `DAppT` r) = l : splitSignature r
splitSignature (DForallT _ t)                = splitSignature t
splitSignature (DSigT t _)                   = splitSignature t
splitSignature t                             = [t]

returnType :: DType -> DType
returnType = last . splitSignature

argTypes :: DType -> [DType]
argTypes = init . splitSignature

-- | Split a type on a head constructor and its parameters
unapply :: DType -> (Name, [DType])
unapply (DConT name) = (name, [])
unapply (DVarT name) = (name, [])
unapply (DForallT _ t) = unapply t
unapply (DSigT t _) = unapply t
unapply (DAppT l r) = (name, l' ++ [r])
  where (name, l') = unapply l
unapply t = unsupported 'unapply t

typeHead :: DType -> Name
typeHead = fst . unapply

typeArgs :: DType -> [DType]
typeArgs = snd . unapply

-- | Is this type a list of things?
isListOf :: DType -> Maybe DType
isListOf (DConT f `DAppT` a) | f == ''[] = Just a
isListOf _                               = Nothing

-- | Is this type a Maybe thing?
isMaybeOf :: DType -> Maybe DType
isMaybeOf (DConT f `DAppT` a) | f == ''Maybe = Just a
isMaybeOf _                                  = Nothing

----------------------------------------
-- | Observations over TyVarBndrs

dTyVarBndrName :: DTyVarBndrUnit -> Name
dTyVarBndrName (DPlainTV tv _)    = tv
dTyVarBndrName (DKindedTV tv _ _) = tv

dTyVarBndrToDTypeArg :: DTyVarBndrUnit -> DTypeArg
dTyVarBndrToDTypeArg (DPlainTV tv _)    = DTANormal (DVarT tv)
dTyVarBndrToDTypeArg (DKindedTV tv _ _) = DTANormal (DVarT tv)

----------------------------------------
-- | Observations over DCons

dConName :: DCon -> Name
dConName (DCon _ _ name _ _) = name

dConFields :: DCon -> DConFields
dConFields (DCon _ _ _ conFields _) = conFields

dConFieldsTypes :: DConFields -> [DType]
dConFieldsTypes (DNormalC _ bts) = map snd bts
dConFieldsTypes (DRecC bts)      = map (\(_,_,t) -> t) bts

dConFieldsNum :: DConFields -> Int
dConFieldsNum (DNormalC _ bts) = length bts
dConFieldsNum (DRecC bts)      = length bts

----------------------------------------
-- | Pure builders

-- | Apply a list of types to a head type constructor
applyDTypes :: Name -> [DTypeArg] -> DType
applyDTypes name args = DConT name `applyDType` args

-- | Apply a list of type vars to a head constructor
applyTVs :: Name -> [DTyVarBndrUnit] -> DType
applyTVs tn vs = applyDTypes tn (fmap dTyVarBndrToDTypeArg vs)

-- | Apply a constructor name to a list of field expressions
mkConDExp :: Name -> [DExp] -> DExp
mkConDExp name = foldl DAppE (DConE name)

-- | Build an applicative expression by chaining `<*>`
mkApplicativeDExp :: Name -> [DExp] -> DExp
mkApplicativeDExp headName = foldl appExp pureExp
  where pureExp = DVarE 'pure `DAppE` DConE headName
        appExp l r = DVarE '(<*>) `DAppE` l `DAppE` r

-- | Build a list expression by chaining `(:)`
mkListDExp :: [DExp] -> DExp
mkListDExp = foldr consExp nilExp
  where nilExp = DConE '[]
        consExp l r = DConE '(:) `DAppE` l `DAppE` r

----------------------------------------
-- | Impure builders

-- | Create a pattern from a constructor using fresh variable names.
-- Returns the patterns as well as the bound variables.
createDPat :: DCon -> Q ([Name], DPat)
createDPat (DCon _ _ cname cfields _) = do
  pvs <- replicateM (dConFieldsNum cfields) (newName "_v")
  let dpat = DConP cname [] [DVarP pv | pv <- pvs]
  return (pvs, dpat)

----------------------------------------
-- | Error and warning messages

-- | Internal errors
unsupported :: Show a => Name -> a -> b
unsupported funName input =
  error $ "[MUTAGEN]" ++ show funName ++ ": unsupported input:\n" ++ show input

dump :: Ppr a => a -> String
dump = pprint

withColor :: Color -> IO () -> IO ()
withColor color io = do
  setSGR [SetColor Foreground Vivid color]
  io
  setSGR [Reset]

mutagenLog :: MonadIO m => String -> m ()
mutagenLog str = liftIO $ putStrLn $ "[MUTAGEN] " ++ str

mutagenLog' :: MonadIO m => String -> m ()
mutagenLog' str = liftIO $ putStrLn str

mutagenError :: Show a => String -> [a] -> Q b
mutagenError msg inputs = runIO $ do
  withColor Red $ do
    mutagenLog "an error happened:"
    mutagenLog msg
    mutagenLog "input was:"
    forM_ inputs $ \i -> do
      mapM_ mutagenLog' (lines (show i))

  error "MUTAGEN derivation error"

----------------------------------------
-- | Some extra niceties

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM mb th el = do
  b <- mb
  if b then th else el

-- replace :: Int -> [a] -> a -> [a]
replace :: Int -> [a] -> a -> [a]
replace n xs x = take n xs <> [x] <> drop (n+1) xs
