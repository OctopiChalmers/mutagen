module Test.Mutagen.Lazy
  ( __lazy__
  , addEvaluatedPos
  , resetPosRef
  , readPosRef
  , Lazy(..)
  ) where

import Data.IORef
import System.IO.Unsafe

-- For providing some default Lazy instances
import Data.Word

----------------------------------------
-- Collecting evaluated positions

__lazy__ :: [Int] -> a -> a
__lazy__ pos expr =
  unsafePerformIO $ do
    addEvaluatedPos pos
    return expr

{-# INLINE __lazy__ #-}

----------------------------------------
-- IORef too keep track of the evaluated positions

pos_ref :: IORef [[Int]]
pos_ref = unsafePerformIO (newIORef [])

-- Add a new position to the ref
addEvaluatedPos :: [Int] -> IO ()
addEvaluatedPos pos = modifyIORef' pos_ref (reverse pos:)

-- Reset traces
resetPosRef :: IO ()
resetPosRef = modifyIORef' pos_ref (const [])

-- Read traces
readPosRef :: IO [[Int]]
readPosRef = readIORef pos_ref

-- -- Run a computation and obtain its trace
-- withEvaluatedPos :: a -> IO (a, [[Int]])
-- withEvaluatedPos a = do
--   resetPosRef
--   a' <- return a
--   pos <- readPosRef
--   return (a', pos)

----------------------------------------
-- Lazy class

class Lazy a where
  lazy :: a -> a
  lazy = lazy' []

  lazy' :: [Int] -> a -> a

instance Lazy Int where
  lazy' = __lazy__

instance Lazy Double where
  lazy' = __lazy__

instance Lazy Float where
  lazy' = __lazy__

instance Lazy Word8 where
  lazy' = __lazy__

instance Lazy Word16 where
  lazy' = __lazy__

instance Lazy Word32 where
  lazy' = __lazy__

instance Lazy Word64 where
  lazy' = __lazy__

instance Lazy Char where
  lazy' = __lazy__

instance Lazy Bool where
  lazy' = __lazy__

instance Lazy a => Lazy (Maybe a) where
  lazy' pre Nothing =
    __lazy__ pre $
    Nothing
  lazy' pre (Just a) =
    __lazy__ pre $
    Just (lazy' (0:pre) a)

instance Lazy a => Lazy [a] where
  lazy' pre [] =
    __lazy__ pre $
    []
  lazy' pre (x:xs) =
    __lazy__ pre $
    ((lazy' (0:pre) x) : (lazy' (1:pre) xs))


-- Tuple instances

instance (Lazy a, Lazy b) => Lazy (a, b) where
  lazy' pre (a, b) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    )

instance (Lazy a, Lazy b, Lazy c) => Lazy (a, b, c) where
  lazy' pre (a, b, c) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d) => Lazy (a, b, c, d) where
  lazy' pre (a, b, c, d) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e) => Lazy (a, b, c, d, e) where
  lazy' pre (a, b, c, d, e) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    , lazy' (4:pre) e
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f) => Lazy (a, b, c, d, e, f) where
  lazy' pre (a, b, c, d, e, f) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    , lazy' (4:pre) e
    , lazy' (5:pre) f
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g) => Lazy (a, b, c, d, e, f, g) where
  lazy' pre (a, b, c, d, e, f, g) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    , lazy' (4:pre) e
    , lazy' (5:pre) f
    , lazy' (6:pre) g
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h) => Lazy (a, b, c, d, e, f, g, h) where
  lazy' pre (a, b, c, d, e, f, g, h) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    , lazy' (4:pre) e
    , lazy' (5:pre) f
    , lazy' (6:pre) g
    , lazy' (7:pre) h
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h, Lazy i) => Lazy (a, b, c, d, e, f, g, h, i) where
  lazy' pre (a, b, c, d, e, f, g, h, i) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    , lazy' (4:pre) e
    , lazy' (5:pre) f
    , lazy' (6:pre) g
    , lazy' (7:pre) h
    , lazy' (8:pre) i
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h, Lazy i, Lazy j) => Lazy (a, b, c, d, e, f, g, h, i, j) where
  lazy' pre (a, b, c, d, e, f, g, h, i, j) =
    __lazy__ pre $
    ( lazy' (0:pre) a
    , lazy' (1:pre) b
    , lazy' (2:pre) c
    , lazy' (3:pre) d
    , lazy' (4:pre) e
    , lazy' (5:pre) f
    , lazy' (6:pre) g
    , lazy' (7:pre) h
    , lazy' (8:pre) i
    , lazy' (9:pre) j
    )
