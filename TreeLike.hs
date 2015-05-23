{-# LANGUAGE OverloadedStrings #-}

module TreeLike where

import Data.Text hiding (map,zipWith)
import Text.Parsec(SourcePos)
import Data.Monoid((<>))

import Prelude hiding (unlines)

data Tree a = Node {
  rootLabel :: a
  , subForest :: [Tree a]
  , source :: Maybe SourcePos
  } deriving (Eq,Show)

instance Functor Tree where
  fmap f (Node a children source) = Node (f a) (map (fmap f) children) source

type Forest a = [Tree a]

class TreeLike t where
  toTree :: t -> Tree Text

instance TreeLike Bool where
  toTree True = Node "yes" [] Nothing
  toTree False = Node "no" [] Nothing

instance TreeLike Double where
  toTree n = Node (pack $ show n) [] Nothing

instance TreeLike Text where
  toTree t = Node t [] Nothing


-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree Text -> Text
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest Text -> Text
drawForest  = unlines . map drawTree

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
