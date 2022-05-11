{-# LANGUAGE FlexibleContexts #-}
module Test.Mutagen.Tracer.Bitmap
  ( TraceBitmapLog
  , emptyTraceBitmapLog
  , resetTraceBitmapLog
  , registerTraceInTraceBitmapLog
  , dumpTraceBitmapLog
  ) where

import Control.Monad

import Data.Foldable
import Data.Array.IO

import Test.Mutagen.Tracer.Trace

----------------------------------------
-- Edge-based bitmap logs

data TraceBitmapLog = TraceBitmapLog Int (IOUArray Int Bool)

emptyTraceBitmapLog :: Int -> IO TraceBitmapLog
emptyTraceBitmapLog n = TraceBitmapLog (n+1) <$> newArray (0, (n+1)*(n+1)) False

resetTraceBitmapLog :: TraceBitmapLog -> IO ()
resetTraceBitmapLog (TraceBitmapLog _ arr) = do
  (l, u) <- getBounds arr
  forM_ [l .. u] $ \i -> do
    writeArray arr i False

registerTraceInTraceBitmapLog :: Trace -> TraceBitmapLog -> IO Int
registerTraceInTraceBitmapLog (Trace entries) (TraceBitmapLog n arr) = do
  let edges = zip (0:entries) entries
  let flipAndCount (i,j) acc = do
        let idx = i*n + j
        b <- readArray arr idx
        if b
          then return acc
          else writeArray arr idx True >> return (acc+1)
  foldrM flipAndCount 0 edges

dumpTraceBitmapLog :: TraceBitmapLog -> IO ()
dumpTraceBitmapLog (TraceBitmapLog n arr) = do
  putStr "+" >> replicateM_ n (putStr "-") >> putStrLn "+"
  forM_ [0 .. n-1] $ \i -> do
    putStr "|"
    forM_ [0 .. n-1] $ \j -> do
      b <- readArray arr (i*n + j)
      if b then putStr "*" else putStr " "
    putStrLn "|"
  putStr "+" >> replicateM_ n (putStr "-") >> putStrLn "+"
