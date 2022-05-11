{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
{-# NOINLINE traceRef #-}
traceRef :: IORef Trace
traceRef = unsafePerformIO (newIORef (Trace []))

-- Add a new entry to the current trace
addTraceEntry :: Int -> IO ()
addTraceEntry n = modifyIORef' traceRef (\(Trace entries) -> Trace (n:entries))

-- Reset traces
resetTraceRef :: IO ()
resetTraceRef = modifyIORef' traceRef (const (Trace []))

-- Read traces
readTraceRef :: IO Trace
readTraceRef = do
  Trace entries <- readIORef traceRef
  return (Trace (reverse entries))

-- Run a computation and obtain its trace
withTrace :: IO a -> IO (a, Trace)
withTrace io = do
  resetTraceRef
  a <- io
  tr <- readTraceRef
  return (a, tr)
