{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- Parse the csv files used for localisation. Each localisation entry consists
-- of a key followed by several entries, separated by ';'. A 'x' marks eol

module Localisation ( Entry(..), localisationFile) where

import Data.Text as T
import Data.List as L
import Data.Either
import Data.Maybe(mapMaybe)
import Data.Char(isSpace)


data Entry = Entry {
  key :: !Text
  , english :: !Text
  , french :: !Text
  , german :: !Text
  , polish :: !Text
  , spanish :: !Text
  , italian :: !Text
  , hungarian :: !Text
  , czech :: !Text
  , source :: !FilePath
  } deriving Show

instance Eq Entry where
  Entry { key = k1 } == Entry { key = k2 } = k1 == k2

instance Ord Entry where
  Entry { key = k1 } `compare` Entry { key = k2 } = k1 `compare` k2

entry' :: Text → Either Text Entry
entry' line = case T.splitOn ";" line of
  (key:english:french:german:polish:spanish:italian:hungarian:czech:_) → Right Entry {..}
  _ → Left line
  where source = ""

line t = if T.head significant == '#'
         then Nothing
         else Just $ entry' significant
  where significant = T.dropWhile isSpace t

localisationFile :: FilePath → Text → Either [Text] [Entry]
localisationFile file t = if anyFailed
                     then Left $ lefts raw
                     else Right $ L.map (\e → e { source = file } ) $ rights raw
  where raw = mapMaybe line $ T.lines t
        anyFailed = L.any isLeft raw
        
