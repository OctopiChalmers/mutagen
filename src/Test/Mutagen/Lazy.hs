module Test.Mutagen.Lazy
  ( __evaluated__
  , addEvaluatedPos
  , resetPosRef
  , readPosRef
  , Lazy(..)
  ) where

import Data.IORef
import System.IO.Unsafe

-- For providing some default Lazy instances
import Data.Word

import Test.Mutagen.Mutation (Pos)

----------------------------------------
-- Collecting evaluated positions

__evaluated__ :: Pos -> a -> a
__evaluated__ pos expr =
  unsafePerformIO $ do
    addEvaluatedPos pos
    return expr

{-# INLINE __evaluated__ #-}

----------------------------------------
-- IORef too keep track of the evaluated positions

pos_ref :: IORef [Pos]
pos_ref = unsafePerformIO (newIORef [])

-- Add a new position to the ref
addEvaluatedPos :: Pos -> IO ()
addEvaluatedPos pos = modifyIORef' pos_ref (reverse pos : )

-- Reset traces
resetPosRef :: IO ()
resetPosRef = modifyIORef' pos_ref (const [])

-- Read traces
readPosRef :: IO [Pos]
readPosRef = reverse <$> readIORef pos_ref

----------------------------------------
-- Lazy class

class Lazy a where
  lazy :: a -> a
  lazy = lazyNode []

  lazyNode :: Pos -> a -> a

----------------------------------------
-- | Lazy instances
----------------------------------------

instance Lazy Int where
  lazyNode = __evaluated__

instance Lazy Double where
  lazyNode = __evaluated__

instance Lazy Float where
  lazyNode = __evaluated__

instance Lazy Word8 where
  lazyNode = __evaluated__

instance Lazy Word16 where
  lazyNode = __evaluated__

instance Lazy Word32 where
  lazyNode = __evaluated__

instance Lazy Word64 where
  lazyNode = __evaluated__

instance Lazy Char where
  lazyNode = __evaluated__

instance Lazy Bool where
  lazyNode = __evaluated__

instance Lazy a => Lazy (Maybe a) where
  lazyNode pre Nothing =
    __evaluated__ pre $
    Nothing
  lazyNode pre (Just a) =
    __evaluated__ pre $
    Just (lazyNode (0:pre) a)

instance (Lazy a, Lazy b) => Lazy (Either a b) where
  lazyNode pre (Left x) =
    __evaluated__ pre $
    Left (lazyNode (0:pre) x)
  lazyNode pre (Right x) =
    __evaluated__ pre $
    Right (lazyNode (0:pre) x)

instance Lazy a => Lazy [a] where
  lazyNode pre [] =
    __evaluated__ pre $
    []
  lazyNode pre (x:xs) =
    __evaluated__ pre $
    ((lazyNode (0:pre) x) : (lazyNode (1:pre) xs))

-- Tuple instances

instance (Lazy a, Lazy b) => Lazy (a, b) where
  lazyNode pre (a, b) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    )

instance (Lazy a, Lazy b, Lazy c) => Lazy (a, b, c) where
  lazyNode pre (a, b, c) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d) => Lazy (a, b, c, d) where
  lazyNode pre (a, b, c, d) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e) => Lazy (a, b, c, d, e) where
  lazyNode pre (a, b, c, d, e) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    , lazyNode (4:pre) e
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f) => Lazy (a, b, c, d, e, f) where
  lazyNode pre (a, b, c, d, e, f) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    , lazyNode (4:pre) e
    , lazyNode (5:pre) f
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g) => Lazy (a, b, c, d, e, f, g) where
  lazyNode pre (a, b, c, d, e, f, g) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    , lazyNode (4:pre) e
    , lazyNode (5:pre) f
    , lazyNode (6:pre) g
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h) => Lazy (a, b, c, d, e, f, g, h) where
  lazyNode pre (a, b, c, d, e, f, g, h) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    , lazyNode (4:pre) e
    , lazyNode (5:pre) f
    , lazyNode (6:pre) g
    , lazyNode (7:pre) h
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h, Lazy i) => Lazy (a, b, c, d, e, f, g, h, i) where
  lazyNode pre (a, b, c, d, e, f, g, h, i) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    , lazyNode (4:pre) e
    , lazyNode (5:pre) f
    , lazyNode (6:pre) g
    , lazyNode (7:pre) h
    , lazyNode (8:pre) i
    )

instance (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h, Lazy i, Lazy j) => Lazy (a, b, c, d, e, f, g, h, i, j) where
  lazyNode pre (a, b, c, d, e, f, g, h, i, j) =
    __evaluated__ pre $
    ( lazyNode (0:pre) a
    , lazyNode (1:pre) b
    , lazyNode (2:pre) c
    , lazyNode (3:pre) d
    , lazyNode (4:pre) e
    , lazyNode (5:pre) f
    , lazyNode (6:pre) g
    , lazyNode (7:pre) h
    , lazyNode (8:pre) i
    , lazyNode (9:pre) j
    )
