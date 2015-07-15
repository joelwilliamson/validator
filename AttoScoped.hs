-- | This is a reimplementation of Scoped.hs using Attoparsec instead of Parsec.
-- | The hope is that it will allow parsing to proceed significantly faster.
-- | Since the error messages for Atto are typically inferior, the plan is to
-- | run the Parsec parser over any file that fails to parse for better error
-- | messages.
module AttoScoped(
  StatefulParser,
  space,
  literal,
  sep,
  value,
  statefulParseOnly
  ) where

import Tree(Tree(..))
import Scoped (Atom(..))

import Data.Char(isDigit,ord)
import Control.Applicative((<|>))
import Data.Attoparsec.Text(Parser,char,double,isEndOfLine,many',parseOnly,peekChar',satisfy,takeTill,takeWhile,takeWhile1)
import Text.Parsec.Pos(SourcePos,incSourceLine)
import Data.Text(Text,all)
import Data.Monoid((<>))

import Control.Monad.State

import Prelude hiding (all,takeWhile)

type StatefulParser = StateT SourcePos Parser

statefulParseOnly :: StatefulParser a → SourcePos → Text → Either String a
statefulParseOnly p s t = fst <$> parseOnly (runStateT p s) t

singleton x = [Node x [] Nothing]

isNumeric c = isDigit c || c == '.' || c == '-'

nextLine :: StatefulParser ()
nextLine = modify (`incSourceLine` 1)

-- | Recognize any whitespace characters, and handle track line count
space :: StatefulParser Char
space = lift (satisfy sameLine) <|>
        lift (satisfy newLine) <* nextLine
  where sameLine c = c == ' ' || c == '\t' || c == '\r'
        newLine c = c == '\n'

stringLiteral :: StatefulParser Atom
stringLiteral = Label <$> lift (char '"' *> takeWhile (/='"') <* char '"')

-- | Recognize an identifier, number or string literal
literal :: StatefulParser Atom
literal = stringLiteral <|> do
  raw ← {-# SCC "literal-raw-parse" #-} lift $ takeWhile1 (not . special)
  if {-# SCC "literal-is-numeric" #-} all isNumeric raw
    then {-# SCC "literal-parse-numeric" #-} return $ Number $ case parseOnly double raw of
                                Right n → n
                                Left e → error $ show e <> " at AttoScoped.hs:31 with input " <> show raw
    else return $ Label raw
  where special c = {-# SCC "literal-special-p" #-} ord c <= 32 || c == '#' || c == '=' || ord c >= 123

-- Parse everything following a '#' to the eol
comment = takeTill isEndOfLine

-- Whitespace and comments
sep = many' (space *> pure ()
             <|> lift (char '#' *> comment *> pure ()))

eq = lift $ char '='
regularBlockFinish = eq >> sep >> rhs <* sep

value = do
  pos ← get
  l ← literal
  _ ← sep
  r ← regularBlockFinish
  return $ Node l r $ Just pos
  
rhs = block <|> (singleton <$> literal)

-- This is a parser for blocks that never backtracks
block :: StatefulParser [Tree Atom]
block = do
  _ ← lift $ char '{'
  pos ← get
  _ ← sep
  l ← literal
  _ ← sep
  ch ← lift peekChar'
  if ch /= '='
    then (do
             r ← lift $ Number <$> double
             _ ← sep
             _ ← lift $ char '}'
             return [Node l (singleton r) $ Just pos]
         )
    else (do
             r ← regularBlockFinish
             (Node l r (Just pos) :) <$> many' value <* (sep *> lift (char '}'))
         )
