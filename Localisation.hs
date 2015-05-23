{-# LANGUAGE UnicodeSyntax #-}
-- Parse the csv files used for localisation. Each localisation entry consists
-- of a key followed by several entries, separated by ';'. A 'x' marks eol

module Localisation ( Entry(..), localisationFile) where

import Data.Text as T
import Text.Parsec
import Data.List as L
import Data.Either
import Data.Monoid((<>))


data Entry = Entry {
  key :: Text
  , english :: Text
  , french :: Text
  , german :: Text
  , polish :: Text
  , spanish :: Text
  , italian :: Text
  , hungarian :: Text
  , czech :: Text
  , source :: FilePath
  } deriving Show

instance Eq Entry where
  Entry { key = k1 } == Entry { key = k2 } = k1 == k2

instance Ord Entry where
  Entry { key = k1 } `compare` Entry { key = k2 } = k1 `compare` k2

keyP :: Parsec Text u Text
keyP = pack <$> manyTill anyChar ({-(space >> fail "invalid space in localisation key") <|>-} char ';')
text :: Parsec Text u Text
text = pack <$> manyTill anyChar (char ';')

entry = do
  k ← keyP
  e ← text
  f ← text
  g ← text
  p ← text
  s ← text
  i ← text
  h ← text
  c ← text
  _ ← many $ char ';'
  _ ← optionMaybe $ char 'x'
  _ ← spaces
  return $ Entry k e f g p s i h c ""

comment = char '#' >> many anyChar >> return ()
emptyLine = spaces >> eof

optionEither :: Parsec Text u a → Parsec Text u b → Parsec Text u (Either a b)
optionEither p1 p2 = do
  r1 <- optionMaybe p1
  case r1 of
    Nothing → Right <$> p2
    Just x1 → return $ Left x1

line = optionEither (comment <|> emptyLine) entry

localisationFile :: FilePath → Text → Either [ParseError] [Entry]
localisationFile file t = if anyFailed
                     then Left $ lefts raw
                     else Right $ L.map (\e → e { source = file } ) $ rights $ rights raw
  where raw = L.map (\(n,s) → parse line (file<>show n) s) . L.zip ([1..]::[Int]) $ T.lines t
        anyFailed = L.any isLeft raw
        
