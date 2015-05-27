-- | This is a reimplementation of Scoped.hs using Attoparsec instead of Parsec.
-- | The hope is that it will allow parsing to proceed significantly faster.
-- | Since the error messages for Atto are typically inferior, the plan is to
-- | run the Parsec parser over any file that fails to parse for better error
-- | messages.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module AttoScoped(
  StatefulParser,
  space,
  literal,
  sep,
  value,
  block
  ) where

import TreeLike(Tree(..))
import Scoped (Label,Atom(..),Block,Namespace,EventId)

import Data.Char(isAlpha,isDigit,ord)
import Control.Applicative((<|>))
import Data.Attoparsec.Text(Parser,char,double,isEndOfLine,many',notChar,parseOnly,satisfy,takeTill,takeWhile,takeWhile1)
import Text.Parsec.Pos(SourcePos,incSourceLine)
import Data.Text(Text,all)
import Data.Monoid((<>))

import Control.Monad.State

import Prelude hiding (all,takeWhile)

type StatefulParser = StateT SourcePos Parser

singleton x = [Node x [] Nothing]

isNumeric c = isDigit c || c == '.'

-- | Recognize any whitespace characters, and handle track line count
space :: StatefulParser Char
space = lift (satisfy sameLine) <|>
        lift (satisfy newLine) <* modify (flip incSourceLine 1)
  where sameLine c = c == ' ' || c == '\t' || c == '\r'
        newLine c = c == '\n'

stringLiteral :: StatefulParser Atom
stringLiteral = Label <$> lift (char '"' *> takeWhile (/='"') <* char '"')

-- | Recognize an identifier, number or string literal
literal :: StatefulParser Atom
literal = stringLiteral <|> do
  raw ← lift $ takeWhile1 (not . special)
  if all isNumeric raw
    then return $ Number $ case parseOnly double raw of
                                Right n → n
                                Left e → error $ show e <> " at AttoScoped.hs:31 with input " <> show raw
    else return $ Label raw
  where special c = ord c <= 32 || c == '#' || c == '=' || ord c >= 123

-- Parse everything following a '#' to the eol
comment = takeTill isEndOfLine

-- Whitespace and comments
sep = many' (space *> pure ()
             <|> lift (char '#' *> comment *> pure ()))

value = do
  pos ← get
  l ← literal
  _ ← sep
  _ ← lift $ char '='
  _ ← sep
  r ← rhs
  return $ Node l r $ Just pos
  
rhs = block <|> (singleton <$> literal)

spawnBlock :: StatefulParser [Block]
spawnBlock = do
  _ ← lift $ char '{'
  _ ← sep
  l ← literal
  _ ← sep
  r ← literal
  _ ← sep
  _ ← lift $ char '}'
  return $ [Node l (singleton r) Nothing]

regularBlock :: StatefulParser [Block]
regularBlock = do
  _ ← lift $ char '{'
  _ ← sep
  elems ← many' (do
    l ← literal
    _ ← sep
    _ ← lift $ char '='
    _ ← sep
    r ← rhs
    _ ← sep
    return $ Node l r Nothing) :: StatefulParser [Block]
  _ ← lift $ char '}'
  return elems

block :: StatefulParser [Block]
block = spawnBlock <|> regularBlock

eventId = (,) <$> takeWhile (\c → isAlpha c || c == '_') <*> double
