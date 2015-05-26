{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoped {-(
  Label(),
  Block(),
  Namespace(),
  EventId(),
  Error(),
  SourcePos(),
  ws,Scoped.dot,number,label,intLit,
  parseValue,eventId,
  quickParse,
  quickDraw,
  lookup,
  lookupWithError,
  getValue,
  getTree,
  getPos,
  lookupAll,
  filterChildren,
  makeCompound
  )-}
       where

--import Data.Tree
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List as L(find,filter,foldl')
import TreeLike
--import Data.Attoparsec.Text(char,skipWhile,isEndOfLine,endOfLine,takeWhile1,double)
import Data.Char(isSpace,ord)
import Text.Parsec hiding ((<|>),label)
import Data.String(IsString,fromString)

import Prelude hiding (lookup)

import Data.Monoid
import Control.Applicative((<|>))

type Label = T.Text
data Atom = Label Label
          | Number Double
            deriving (Eq,Ord,Show)
type Block = Tree Atom
type Namespace = Label
type EventId = (Namespace,Double)

type Error = (T.Text, Maybe SourcePos)

instance Data.String.IsString Atom where
  fromString = Label . fromString
             
{-
identChar c = (not $ isSpace c) && (c /= '{') && (c /= '}') && (c /= '=')

comment = char '#' *> skipWhile (not . isEndOfLine) *> endOfLine
identifier = takeWhile1 identChar
lhs = Label <$> identifier <|> Number <$> double
rhs = Label 
             -}

singleton a s = [Node a [] $ Just s]
{-
scopedLanguage = LanguageDef {
  commentStart = "",
  commentEnd = "",
  commentLine = "#",
  nestedComments = False,
  identStart =  alphaNum <|> oneOf "_.:/+-![]'?",
  identLetter = alphaNum <|> oneOf "_.:/+-![]'?",
  opStart = char '=' <|> char '-',
  opLetter = choice [],
  reservedNames = [],
  reservedOpNames = ["=","-"],
  caseSensitive = True
  }

scopedParser :: GenTokenParser T.Text u Identity
scopedParser = makeTokenParser scopedLanguage

--equal = reservedOp scopedParser "="
neg = reservedOp scopedParser "-"
nested = braces scopedParser
label = pack <$> identifier scopedParser
ws = whiteSpace scopedParser
intLit = integer scopedParser
stringLit = pack <$> stringLiteral scopedParser
dot = Text.Parsec.Token.dot scopedParser-}

identChar :: Parsec T.Text u Char
identChar = alphaNum <|> oneOf "_.:/+-![]'?"
stringLit = Label . T.pack <$> (char '"' *> many (noneOf "\"") <* char '"')
isEndOfLine c = c == '\r' || c == '\n'
comment :: Parsec T.Text u ()
comment = char '#' *> manyTill anyChar (endOfLine *> pure () <|> eof) *> pure ()
sep = spaces *> many (comment *> spaces)
equal = char '='
fractionalPart accum factor= do
  d ← optionMaybe digit
  case d of
    Nothing → return accum
    Just n → fractionalPart (accum + factor* conv n) (factor/10)
    where conv n = fromIntegral $ ord n - 48
decimal = foldl' step 0 <$> many1 digit
  where step a c = a * 10 + fromIntegral (ord c - 48)
number = Number <$> do
  positive ← (False <$ char '-') <|> pure True
  n ← decimal
  f ← option 0 (char '.' *> fractionalPart 0 (0.1))
  if positive
    then return $ fromIntegral n + f
    else return (-(fromIntegral n + f))
label = Label . T.pack <$> many1 identChar
lhs = try number <|> label
rhs = singleton <$> (try number <|> label <|> stringLit) <*> getPosition
      <|> block
value = do
  pos ← getPosition
  l ← lhs
  _ ← sep
  try (equal *> sep *> rhs <* sep >>= \r →
        return $ Node { rootLabel = l, subForest = r, source = Just pos })
    <|> (number <* sep >>= \n →
         return $ Node { rootLabel = l, subForest = singleton n pos, source = Just pos})
block :: Parsec T.Text u [Tree Atom]
block = do
  _ ← char '{'
  _ ← sep
  contents ← many value
  _ ← sep
  _ ← char '}'
  return $ contents

eventId = (,) <$> option "" (T.pack <$> many1 (letter <|> char '_') <* char '.') <*> number

parseValue = value
parseBlock = block
      
{-
number = do
  n <- optionMaybe neg
  v <- naturalOrFloat scopedParser
  return $ case (n,v) of
    (Nothing,Left i) → fromIntegral i
    (Just _,Left i) → negate $ fromIntegral i
    (Nothing,Right f) → f
    (Just _,Right f) → negate f

eventId :: Parsec T.Text u EventId
eventId = (,) <$> option "" ((T.pack <$> many1 (letter <|> char '_')) <* Scoped.dot) <*> intLit


-}
quickParse p s = res
  where res = case parse p "quickParse" s of
          (Right x) -> x
          (Left err) -> error $ show err

quickDraw :: Show a => Tree a -> IO ()
quickDraw t = TIO.putStr $ drawTree $ (T.pack . show) <$> t
{-
parseValue :: Parsec T.Text u (Tree Atom)
parseValue =
  try (do
          -- These are used in spawn_unit/spawn_fleet
          n1 <- number
          s1 <- getPosition
          n2 <- number
          s2 <- getPosition
          return $ Node n1 [Node n2 [] $ Just s2] $ Just s1
      )
  <|> (do
          l <- label
          equal
          v <- parseBlock
          s <- getPosition
          return $ Node l v $ Just s
      )

parseBlock :: Parsec T.Text u [Tree Atom]
parseBlock = parseLabel <|> parseStringLit <|> parseNumber <|> nested (many parseValue)
  where parseLabel = do
          l <- label
          s <- getPosition
          return [singleton l s]
        parseStringLit = do
          str <- stringLit
          s <- getPosition
          return [singleton str s]
        parseNumber = do
          num <- number
          s <- getPosition
          return [singleton num s]
-}
getValue :: Tree a -> a
getValue Node { subForest }  = rootLabel $ head subForest

getTree :: Eq a ⇒ a → Tree a → Maybe (Tree a)
getTree k Node {..} = L.find p subForest
  where p Node {..} = rootLabel == k

lookup :: Eq a => a -> Tree a -> Maybe a
lookup k Node {..} = getValue <$> L.find p subForest
  where p Node {..} = rootLabel == k

withError :: a → Maybe b → Either a b
withError err Nothing = Left err
withError _ (Just r) = Right r

lookupWithError :: (Eq a,Show a) ⇒ a → Tree a → Either Error a
lookupWithError v t = withError ("Couldn't find " <> T.pack (show v), source t) $ lookup v t

lookupAll :: Eq a => a -> Tree a -> [Tree a]
lookupAll k Node { subForest } = L.filter ((==) k . rootLabel) subForest

filterChildren :: (a -> Bool) -> Tree a -> [Tree a]
filterChildren p Node { subForest } = L.filter (p . rootLabel) subForest

makeCompound maker name t = case getTree name t of
  Just Node {..} → sequence $ maker <$> subForest
  Nothing → Right []

getPos Node { source } = source
