-- | This is basically the same as Data.Tree, except each node can carry
-- information about where in the file it was created.
module Tree
       (
         Tree(..)
       , drawTree
       ) where

import Data.Text hiding (map,zipWith)
import Text.Parsec(SourcePos)
import Data.Monoid((<>))

import Prelude hiding (unlines)

-- | A multi-way tree
data Tree a = Node {
  rootLabel :: a -- ^ The label of this node
  , subForest :: [Tree a] -- ^ Zero or more children
  , source :: Maybe SourcePos -- ^ A source location corresponding to this node
  } deriving (Eq,Show)

instance Functor Tree where
  fmap f (Node a children source) = Node (f a) (map (fmap f) children) source


-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree Text -> Text
drawTree  = unlines . draw

draw :: Tree Text -> [Text]
draw (Node x ts0 _) = x : drawSubTrees ts0
  where
    drawSubTrees :: [Tree Text] -> [Text]
    drawSubTrees [] = []
    drawSubTrees [t] =
      "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
      "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift :: Text -> Text -> [Text] -> [Text]
    shift first other = zipWith (<>) (first : repeat other)
