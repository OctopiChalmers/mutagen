{-# LANGUAGE TypeFamilies #-}
module Tracer.TraceLogger where

import Tracer.Trace
import Tracer.Tree
import Tracer.Bitmap

----------------------------------------
-- A type class for trace stores

class TraceLogger log where
  type RegisterRes log :: *
  emptyTraceLog :: Int -> IO log
  resetTraceLog :: log -> IO ()
  registerTrace :: Trace -> log -> IO (RegisterRes log)
  dumpTraceLog :: log -> IO ()

instance TraceLogger TraceTreeLog where
  type RegisterRes TraceTreeLog = (Int, Int)
  emptyTraceLog = const emptyTraceTreeLog
  resetTraceLog = resetTraceTreeLog
  registerTrace = registerTraceInTraceTreeLog
  dumpTraceLog = dumpTraceTreeLog

instance TraceLogger TraceBitmapLog where
  type RegisterRes TraceBitmapLog = Int
  emptyTraceLog = emptyTraceBitmapLog
  resetTraceLog = resetTraceBitmapLog
  registerTrace = registerTraceInTraceBitmapLog
  dumpTraceLog = dumpTraceBitmapLog
