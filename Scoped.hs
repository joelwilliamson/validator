{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoped (
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
  )
       where

--import Data.Tree
import Data.Text hiding (singleton,head)
import qualified Data.Text.IO as TIO
import Text.Parsec hiding (label)
import Data.List as L(find,filter)
import Text.Parsec.Token
import TreeLike

import Prelude hiding (lookup)

import Data.Monoid
import Data.Functor.Identity

type Label = Text
type Block = Tree Label
type Namespace = Label
type EventId = (Namespace,Integer)
type Error = (Text,Maybe SourcePos)

singleton a s = Node a [] $ Just s

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
                             caseSensitive = True}

scopedParser :: GenTokenParser Text u Identity
scopedParser = makeTokenParser scopedLanguage

equal = reservedOp scopedParser "="
neg = reservedOp scopedParser "-"
nested = braces scopedParser
label = pack <$> identifier scopedParser
ws = whiteSpace scopedParser
intLit = integer scopedParser
stringLit = pack <$> stringLiteral scopedParser
dot = Text.Parsec.Token.dot scopedParser

number = do
  n <- optionMaybe neg
  v <- naturalOrFloat scopedParser
  return $ case (n,v) of
    (Nothing,Left i) → fromIntegral i
    (Just _,Left i) → negate $ fromIntegral i
    (Nothing,Right f) → f
    (Just _,Right f) → negate f

eventId :: Parsec Text u EventId
eventId = (,) <$> option "" ((pack <$> many1 (letter <|> char '_')) <* Scoped.dot) <*> intLit


quickParse p s = res
  where res = case parse p "" s of
          (Right x) -> x
          (Left err) -> error $ show err

quickDraw :: Show a => Tree a -> IO ()
quickDraw t = TIO.putStr $ drawTree $ (pack . show) <$> t

parseValue :: Parsec Text u (Tree Text)
parseValue =
  try (do
          n1 <- number
          s1 <- getPosition
          n2 <- number
          s2 <- getPosition
          return $ Node (pack $ show n1) [Node (pack $ show n2) [] $ Just s2] $ Just s1
      )
  <|> (do
          l <- label
          equal
          v <- parseBlock
          s <- getPosition
          return $ Node l v $ Just s
      )

parseBlock :: Parsec Text u [Tree Text]
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
          return [singleton (pack . show $ num) s]

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
lookupWithError v t = withError ("Couldn't find " <> pack (show v), source t) $ lookup v t

lookupAll :: Eq a => a -> Tree a -> [Tree a]
lookupAll k Node { subForest } = L.filter ((==) k . rootLabel) subForest

filterChildren :: (a -> Bool) -> Tree a -> [Tree a]
filterChildren p Node { subForest } = L.filter (p . rootLabel) subForest

makeCompound maker name t = case getTree name t of
  Just Node {..} → sequence $ maker <$> subForest
  Nothing → Right []

getPos Node { source } = source
