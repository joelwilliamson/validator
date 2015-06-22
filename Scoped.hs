{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- This is needed to silence all the
-- warnings about Parsec parsers not being exported.
-- | Define the base types for the raw trees output by the parsing phase. This
-- module also contains a Parsec based parser, but this is not currently used,
-- as the Attoparsec parser is marginally faster.
module Scoped (
  Label(),
  Error(),
  Atom(Label,Number),
  EventId(),
  eventId,
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List as L(find,filter,foldl')
import Tree(Tree(..),drawTree)
import Data.Char(ord)
import Text.Parsec hiding ((<|>),label)
import Data.String(IsString,fromString)

import Prelude hiding (lookup)

import Data.Monoid((<>))
import Control.Applicative((<|>))

-- | A synonym for text
type Label = T.Text

-- | Every node in the raw Tree is labeled with an atom.
data Atom = Label Label
          | Number Double
            deriving (Eq,Ord,Show)

-- | Each namespace can contain several events. They are equivalent to unique
-- numbers, but make it easier to avoid conflicting event ids.
type Namespace = Label

-- | A unique identifier for an event. If it is a non-namespace event, the
-- namespace will be "".
type EventId = (Namespace,Double)

-- | Describe an error and where it occured
type Error = (T.Text, Maybe SourcePos)

instance Data.String.IsString Atom where
  fromString = Label . fromString
             
singleton a s = [Node a [] $ Just s]

identChar :: Parsec T.Text u Char
identChar = satisfy (not . special) <?> "identifier character"
  where special c = ord c <= 32 || c == '#' || c == '=' || c == '"' || ord c >= 123
stringLit = Label . T.pack <$> (char '"' *> many (noneOf "\"") <* char '"')
isEndOfLine c = c == '\r' || c == '\n'
comment :: Parsec T.Text u ()
comment = char '#'
          *> skipMany (satisfy (not . isEndOfLine))
          *> (endOfLine *> pure () <|> eof) *> pure ()
sep = many (comment <|> space *> spaces)
equal :: Parsec T.Text u Char
equal = satisfy (=='=')
fractionalPart accum factor = do
  d ← optionMaybe digit
  case d of
    Nothing → return accum
    Just n → fractionalPart (accum + factor* conv n) (factor/10)
    where conv n = fromIntegral $ ord n - 48

decimal :: Parsec T.Text u Integer
decimal = foldl' step 0 <$> many1 digit
  where step a c = a * 10 + fromIntegral (ord c - 48)
number = Number <$> do
  positive ← (False <$ char '-') <|> pure True
  n ← decimal
  f ← option 0 (char '.' *> fractionalPart 0 0.1)
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
  _ ← char '='
  _ ← sep
  r ← rhs
  _ ← sep
  return $ Node l r $ Just pos
block :: Parsec T.Text u [Tree Atom]
block = try (do
                _ ← char '{'
                _ ← sep
                pos ← getPosition
                l ← number
                _ ← sep
                r ← number
                _ ← sep
                _ ← char '}'
                return [Node l (singleton r pos) $ Just pos]
            ) <|> (do
                      _ ← char '{'
                      _ ← sep
                      contents ← many value
                      _ ← sep
                      _ ← char '}'
                      return contents
                  )

-- | Parse an event id with Parsec
eventId = (,) <$> option "" (T.pack <$> many1 (letter <|> char '_') <* char '.') <*> number

quickParse p s = res
  where res = case parse p "quickParse" s of
          (Right x) -> x
          (Left err) -> error $ show err

quickDraw :: Show a => Tree a -> IO ()
quickDraw t = TIO.putStr $ drawTree $ (T.pack . show) <$> t

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
