module Test.Mutagen.Position where

import Data.Tree

----------------------------------------
-- | Breadcrumbs used to find subexpressions inside of values

type Pos = [Int]

----------------------------------------
-- | A class for types with indexable positions

class Positional a where
  positions :: a -> Tree Pos


-- Helpers

node :: [(Int, Tree Pos)] -> Tree Pos
node xs = Node [] (fmap (\(idx, children) -> fmap (idx:) children) xs)

invalidPosition :: Pos -> a
invalidPosition pos = error ("inside: invalid position: " <> show pos)

invalidPositionShow :: Show a => Pos -> a -> b
invalidPositionShow pos a = error ("inside: invalid position: " <> show pos <> "\nvalue: " <> show a)
