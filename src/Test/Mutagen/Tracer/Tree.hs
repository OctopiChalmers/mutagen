module Test.Mutagen.Tracer.Tree
  ( TraceTreeLog
  , emptyTraceTreeLog
  , resetTraceTreeLog
  , registerTraceInTraceTreeLog
  , dumpTraceTreeLog
  ) where

import Data.IORef

import Data.Tree
import Data.Tree.Pretty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Test.Mutagen.Tracer.Trace

----------------------------------------
-- Tree-based trace logs

newtype TraceTreeLog = TraceTreeLog (IORef TraceTree)

emptyTraceTreeLog :: IO TraceTreeLog
emptyTraceTreeLog = TraceTreeLog <$> newIORef emptyTraceTree

resetTraceTreeLog :: TraceTreeLog -> IO ()
resetTraceTreeLog (TraceTreeLog ref) = writeIORef ref emptyTraceTree

-- Insert a trace into a trace tree:
--
-- Returns a new log that incorporates this execution, along with:
-- the number of new trace entries executed by this trace (if any)
-- the depth where the new trace entries where inserted

registerTraceInTraceTreeLog :: Trace -> TraceTreeLog -> IO (Int, Int)
registerTraceInTraceTreeLog trace (TraceTreeLog ref) = do
  tt <- readIORef ref
  let (tt', n, d) = insertTrace trace tt
  writeIORef ref tt'
  return (n, d)

dumpTraceTreeLog :: TraceTreeLog -> IO ()
dumpTraceTreeLog (TraceTreeLog ref) = readIORef ref >>= drawTraceTree >>= putStrLn

----------------------------------------
-- Trace trees implemented using "rose maps"

newtype TraceTree = TraceTree (Map TraceEntry TraceTree)

emptyTraceTree :: TraceTree
emptyTraceTree = TraceTree mempty

insertTrace :: Trace -> TraceTree -> (TraceTree, Int, Int)
insertTrace (Trace entries) = go 0 entries
  where
    go d []     (TraceTree tt) = (TraceTree tt, 0, d)
    go d (e:es) (TraceTree tt) =
      case Map.lookup e tt of
        Nothing ->
          let (subTree, new) = chain es
          in (TraceTree (Map.insert e subTree tt), new+1, d)
        Just subTree ->
          let (subTree', new, d') = go (d+1) es subTree
          in (TraceTree (Map.insert e subTree' tt), new, d')

    chain [] = (TraceTree mempty, 0)
    chain (e:es) = (TraceTree (Map.singleton e tlog'), n+1)
      where (tlog', n) = chain es

traceTreetoForest :: TraceTree -> Forest Int
traceTreetoForest (TraceTree tt) =
  Map.elems (Map.mapWithKey (\e tlog' -> Node e (traceTreetoForest tlog')) tt)

drawTraceTree :: TraceTree -> IO String
drawTraceTree tt = return (drawVerticalForest (fmap show <$> traceTreetoForest tt))
