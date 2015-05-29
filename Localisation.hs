{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- Parse the csv files used for localisation. Each localisation entry consists
-- of a key followed by several entries, separated by ';'. A 'x' marks eol

module Localisation ( Entry(..), localisationFile) where

import Data.Text as T(Text,pack)
import Data.Either
import Data.Monoid((<>))
import Control.Applicative(many)
import Data.Attoparsec.Text as A(char,eitherP,endOfLine,isEndOfLine,isHorizontalSpace,many1,many',option,parseOnly,satisfy,sepBy',space,takeTill,takeWhile,takeWhile1)


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

text = A.takeWhile (/=';') <* char ';'
comment = A.takeTill isEndOfLine

entry = do
  key ← A.takeWhile1 (\c → not (isHorizontalSpace c) && c /=';' && c /= '#') <* char ';'
  english ← text
  french ← text
  german ← text
  polish ← text
  spanish ← text
  italian ← text
  hungarian ← text
  czech ← text
  let source = ""
  return Entry {..}

line = eitherP (entry <* A.takeTill isEndOfLine) comment

localisationFile ::FilePath → Text → Either Text [Entry]
localisationFile file t = case parseOnly (sepBy' line (many1 endOfLine) <* (many $ satisfy isEndOfLine)) t of
  Left e → Left $ "Error in localisation file " <> pack file <> ": " <> pack e
  Right es → Right $ lefts es
{-
entry' :: Text → Either Text Entry
entry' line = case T.splitOn ";" line of
  (key:english:french:german:polish:spanish:italian:hungarian:czech:_) → Right Entry {..}
  _ → Left line
  where source = ""

line t = if T.null signficant || T.head significant == '#'
         then Nothing
         else Just $ entry' significant
  where significant = T.dropWhile isSpace t

localisationFile :: FilePath → Text → Either [Text] [Entry]
localisationFile file t = if anyFailed
                     then Left $ lefts raw
                     else Right $ L.map (\e → e { source = file } ) $ rights raw
  where raw = mapMaybe line $ T.lines t
        anyFailed = L.any isLeft raw
        
-}
