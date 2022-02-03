{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Mutagen.Tracer.Trace where

import Data.IORef
import System.IO.Unsafe

----------------------------------------
-- | Traces

-- Trace entries: module name and a uid within that module
type TraceEntry = Int

newtype Trace = Trace { unTrace :: [TraceEntry] }
  deriving Show

-- The dynamic traces are stored into this IORef
{-# NOINLINE trace_ref #-}
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
