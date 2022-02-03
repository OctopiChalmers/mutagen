{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Mutagen.Tracer.Plugin
  ( __trace__
  , TraceAnn(TRACE)
  , plugin
  ) where

import Control.Monad

import Data.IORef
import Data.Generics (mkM, everywhereM, listify, Data)

import System.IO.Unsafe ( unsafePerformIO )

import GHC.Plugins hiding ((<>))
import GHC.Hs
import GHC.Types.Name.Occurrence as Name

import Test.Mutagen.Tracer.Trace

----------------------------------------
-- | Tracing primitive

__trace__ :: Int -> a -> a
__trace__ n expr =
  unsafePerformIO $ do
    addTraceEntry n
    return expr

{-# INLINE __trace__ #-}

----------------------------------------
-- | Source plugin

data TraceAnn = TRACE
  deriving Data

-- IORefs

{-# NOINLINE uid #-}
uid :: IORef Int
uid = unsafePerformIO (newIORef 0)

newUID :: Hsc Int
newUID = liftIO $ atomicModifyIORef uid (\n -> (n+1, n+1))

-- The top-level plugin
plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = tracePlugin }

-- The plugin logic
tracePlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
tracePlugin _cli summary source = do
  -- Initialize some stuff
  flags <- getDynFlags
  let modName = moduleName (ms_mod summary)
  -- Apply some transformations over the source code
  message $ "plugin started on module " <> showPpr flags modName
  let L loc hsMod = hpm_module source
  -- Check if there are TRACE annotations in the code.
  -- If not, transform the whole module.
  case extractAnn <$> listify (isAnn flags) hsMod of
    [] -> do
      message $ "run mode: full module"
      let transform = addTraceImport flags modName
                      >=> everywhereM (mkM (annotateGRHS flags))
                      >=> everywhereM (mkM (annotateIfs  flags))
      hsMod' <- transform hsMod
      n <- liftIO $ readIORef uid
      liftIO $ writeFile ".tracer" (show n)
      message $ "generated " <> show n <> " trace nodes"
      message $ "done"
      return (source { hpm_module = L loc hsMod' })
    anns -> do
      message $ "run mode: trace only " <> showPpr flags anns
      let transform = addTraceImport flags modName
                      >=> everywhereM (mkM (annotateTopLevel flags anns))
      hsMod' <- transform hsMod
      n <- liftIO $ readIORef uid
      liftIO $ writeFile ".tracer" (show n)
      message $ "generated " <> show n <> " trace nodes"
      message $ "done"
      return (source { hpm_module = L loc hsMod' })

-- Include an import to this module, so __trace__ is always in scope
addTraceImport :: DynFlags -> ModuleName -> HsModule -> Hsc HsModule
addTraceImport flags modName hsMod = do
  message $ "adding tracer import to module " <> showPpr flags (moduleNameFS modName)
  let theNewImport = pluginLoc (simpleImportDecl this_module_name)
  let hsMod' = hsMod { hsmodImports = theNewImport : hsmodImports hsMod }
  return hsMod'

-- Annotate every RHS with a tracer
-- They come after: function clauses, case statements, multi-way ifs, etc
annotateGRHS :: DynFlags -> GRHS GhcPs (LHsExpr GhcPs) -> Hsc (GRHS GhcPs (LHsExpr GhcPs))
annotateGRHS flags (GRHS ext guards body) = do
  nth <- newUID
  instrumentedMessage flags "rhs" nth (getLoc body)
  let body' = wrapTracer nth body
  return (GRHS ext guards body')

-- Annotate each branch of an if-then-else expression with a tracer
annotateIfs :: DynFlags -> HsExpr GhcPs -> Hsc (HsExpr GhcPs)
annotateIfs flags expr =
  case expr of
    HsIf ext cond th el -> do
      -- then branch
      nth <- newUID
      instrumentedMessage flags "then branch" nth (getLoc th)
      let th' = wrapTracer nth th
      -- else branch
      nel <- newUID
      instrumentedMessage flags "else branch" nel (getLoc el)
      let el' = wrapTracer nel el
      -- wrap it up again
      return (HsIf ext cond th' el')
    x -> return x

-- Annotate top level functions having TRACE annotation pragmas
annotateTopLevel :: DynFlags -> [RdrName] -> Match GhcPs (LHsExpr GhcPs) -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateTopLevel flags anns match =
  case match of
    Match m_x m_ctx m_ps m_bodies
      | isFunRhs m_ctx && unLoc (mc_fun m_ctx) `elem` anns -> do
          let transform = everywhereM (mkM (annotateGRHS flags))
                          >=> everywhereM (mkM (annotateIfs flags))
          m_bodies' <- transform m_bodies
          return (Match m_x m_ctx m_ps m_bodies')
    x -> return x

----------------------------------------
-- | Helpers

message :: String -> Hsc ()
message str = liftIO $ putStrLn $ "[MUTAGEN] " <> str

instrumentedMessage :: DynFlags -> String -> Int -> SrcSpan -> Hsc ()
instrumentedMessage flags reason n loc = do
  message $ "inoculating tracer #" <> show n <>
            " on " <> reason <>
            " at " <> showPpr flags loc

-- Wrap an expression with a tracer
wrapTracer :: Int -> LHsExpr GhcPs -> LHsExpr GhcPs
wrapTracer n expr =
  var tracer_fun_name
  `app` numLit n
  `app` paren expr

-- Check whether an annotation pragma is of the shape:
-- {-# ANN ident TRACE #-}

pattern HsAnn :: RdrName -> RdrName -> AnnDecl GhcPs
pattern HsAnn lhs rhs <-
  HsAnnotation _ _
  (ValueAnnProvenance (L _ lhs))
  (L _ (HsVar _ (L _ rhs)))

isAnn :: DynFlags -> AnnDecl GhcPs -> Bool
isAnn flags (HsAnn _ rhs) = showPpr flags rhs == showPpr flags trace_ann_name
isAnn _     _             = False

extractAnn :: AnnDecl GhcPs -> RdrName
extractAnn (HsAnn target _) = target
extractAnn _                = error "this should not happen"

-- Is this a patter matching an argument of a function binding?
isFunRhs :: HsMatchContext id -> Bool
isFunRhs (FunRhs {}) = True
isFunRhs _           = False

----------------------------------------
-- | Constant names

tracer_fun_name :: RdrName
tracer_fun_name = mkRdrName "__trace__"

trace_ann_name :: RdrName
trace_ann_name = mkRdrName "TRACE"

this_module_name :: ModuleName
this_module_name = mkModuleName "Test.Mutagen.Tracer.Plugin"

----------------------------------------
-- | Builders

mkRdrName :: String -> RdrName
mkRdrName str = mkUnqual Name.varName (mkFastString str)

pluginLoc :: a -> Located a
pluginLoc = L (srcLocSpan generatedSrcLoc)

var :: RdrName -> LHsExpr GhcPs
var v = pluginLoc (HsVar noExtField (pluginLoc v))

app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app x y = pluginLoc (HsApp noExtField x y)

infixl 5 `app`

paren :: LHsExpr GhcPs -> LHsExpr GhcPs
paren e = pluginLoc (HsPar noExtField e)

numLit :: Int -> LHsExpr GhcPs
numLit n = pluginLoc (HsLit noExtField (HsInt noExtField (mkIntegralLit n)))
