{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Tracer 
  ( __tracer__
  , TraceEntry
  , Trace(..)
  , withTrace
  , addTraceEntry
  , readTraceRef
  , resetTraceRef
  , TraceLog
  , emptyTraceLog
  , registerTrace
  , toForest
  , drawTraceLog
  , drawTraceLog'
  , TraceAnn(TRACE)
  , plugin
  ) where

import Data.IORef
import Data.Generics (mkM, everywhereM, listify, Data)
import System.IO.Unsafe
import Control.Monad

import Data.Tree
import Data.Tree.Pretty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GhcPlugins hiding ((<>))
import GHC.Hs
import OccName as Name

----------------------------------------
-- | Tracing primitive

__tracer__ :: Int -> a -> a
__tracer__ n expr = 
  unsafePerformIO $ do
    addTraceEntry n
    return expr

{-# INLINE __tracer__ #-}

----------------------------------------
-- | Traces

-- Trace entries: module name and a uid within that module
type TraceEntry = Int

newtype Trace = Trace { unTrace :: [TraceEntry] }
  deriving Show

-- The dynamic traces are stored into this IORef 
trace_ref :: IORef Trace
trace_ref = unsafePerformIO (newIORef (Trace []))

-- Add a new entry to the current trace
addTraceEntry :: Int -> IO ()
addTraceEntry n = modifyIORef' trace_ref (\(Trace entries) -> Trace (n:entries))

-- Reset traces
resetTraceRef :: IO ()
resetTraceRef = modifyIORef' trace_ref (const (Trace []))

-- Read traces
readTraceRef :: IO Trace
readTraceRef = do
  Trace entries <- readIORef trace_ref
  return (Trace (reverse entries))

-- Run a computation and obtain its trace
withTrace :: IO a -> IO (a, Trace)
withTrace io = do
  resetTraceRef
  a <- io
  tr <- readTraceRef
  return (a, tr)

----------------------------------------
-- Trace logs

newtype TraceLog = TraceLog (Map TraceEntry TraceLog)

emptyTraceLog :: TraceLog
emptyTraceLog = TraceLog mempty

-- Insert a trace into a trace log:
-- 
-- Returns a new log that incorporates this execution, along with:
-- the number of new trace entries executed by this trace (if any)
-- the depth where the new trace entries where inserted

registerTrace :: Trace -> TraceLog -> (TraceLog, Int, Int)
registerTrace (Trace entries) = go 0 entries
  where
    go d []     (TraceLog tlog) = (TraceLog tlog, 0, d)
    go d (e:es) (TraceLog tlog) =
      case Map.lookup e tlog of
        Nothing ->
          let (subLog, new) = chain es
          in (TraceLog (Map.insert e subLog tlog), new+1, d)
        Just subLog ->
          let (subLog', new, d') = go (d+1) es subLog
          in (TraceLog (Map.insert e subLog' tlog), new, d')

    chain [] = (emptyTraceLog, 0)
    chain (e:es) = (TraceLog (Map.singleton e tlog'), n+1)
      where (tlog', n) = chain es

-- Pretty printing

toForest :: TraceLog -> Forest Int
toForest (TraceLog tlog) = 
  Map.elems (Map.mapWithKey (\e tlog' -> Node e (toForest tlog')) tlog)

drawTraceLog :: TraceLog -> String
drawTraceLog tlog = drawVerticalForest (fmap show <$> toForest tlog)

drawTraceLog' :: TraceLog -> IO ()
drawTraceLog' = putStr . drawTraceLog


----------------------------------------
-- | Source plugin

data TraceAnn = TRACE 
  deriving Data

uid :: IORef Int
uid = unsafePerformIO (newIORef 0)

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
      message $ "done"
      return (source { hpm_module = L loc hsMod' })
    anns -> do
      message $ "run mode: trace only " <> showPpr flags anns
      let transform = addTraceImport flags modName
                      >=> everywhereM (mkM (annotateTopLevel flags anns))
      hsMod' <- transform hsMod
      message $ "done"
      return (source { hpm_module = L loc hsMod' })

-- Include an import to this module, so __tracer__ is always in scope
addTraceImport :: DynFlags -> ModuleName -> HsModule GhcPs -> Hsc (HsModule GhcPs)
addTraceImport flags modName hsMod = do
  message $ "adding tracer import to module " <> showPpr flags (moduleNameFS modName)
  let theNewImport = pluginLoc (simpleImportDecl this_module_name)
  let hsMod' = hsMod { hsmodImports = theNewImport : hsmodImports hsMod }
  return hsMod'

-- Annotate every RHS with a tracer
-- They come after: function clauses, case statements, multi-way ifs, etc
annotateGRHS :: DynFlags -> GRHS GhcPs (LHsExpr GhcPs) -> Hsc (GRHS GhcPs (LHsExpr GhcPs))
annotateGRHS flags grhs =
  case grhs of
    GRHS ext guards body -> do
      n <- liftIO $ atomicModifyIORef uid (\n -> (n+1, n))
      instrumentedMessage flags "rhs" n (getLoc body)
      let body' = wrapTracer n body
      return (GRHS ext guards body')
    x -> return x

-- Annotate each branch of an if-then-else expression with a tracer
annotateIfs :: DynFlags -> HsExpr GhcPs -> Hsc (HsExpr GhcPs)
annotateIfs flags expr =
  case expr of
    HsIf ext sexpr cond th el -> do
      -- then branch
      nth <- newUID uid
      instrumentedMessage flags "then branch" nth (getLoc th)
      let th' = wrapTracer nth th
      -- else branch
      nel <- newUID uid
      instrumentedMessage flags "else branch" nel (getLoc el)
      let el' = wrapTracer nel el
      -- wrap it up again
      return (HsIf ext sexpr cond th' el')
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
message str = liftIO $ putStrLn $ "[Tracer] " <> str

instrumentedMessage :: DynFlags -> String -> Int -> SrcSpan -> Hsc ()
instrumentedMessage flags reason n loc = do
  message $ "inoculating tracer #" <> show n <>
            " on " <> reason <> 
            " at " <> showPpr flags loc

newUID :: IORef Int -> Hsc Int
newUID ref = liftIO $ atomicModifyIORef ref (\n -> (n+1, n))

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
tracer_fun_name = mkRdrName "__tracer__"

trace_ann_name :: RdrName
trace_ann_name = mkRdrName "TRACE"

this_module_name :: ModuleName
this_module_name = mkModuleName "Tracer"

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
