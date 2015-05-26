-- | This is a reimplementation of Scoped.hs using Attoparsec instead of Parsec.
-- | The hope is that it will allow parsing to proceed significantly faster.
-- | Since the error messages for Atto are typically inferior, the plan is to
-- | run the Parsec parser over any file that fails to parse for better error
-- | messages.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module AttoScoped where

import TreeLike(Tree(..))
import Scoped (Label,Atom(..),Block,Namespace,EventId)

import Data.Char(isAlpha,isDigit,ord)
import Control.Applicative((<|>))
import Data.Attoparsec.Text
import Data.Text(all)

import Prelude hiding (all,takeWhile)

singleton x = [Node x [] Nothing]

isNumeric c = isDigit c || c == '.'

literal = do
  raw ← takeTill special
  if all isNumeric raw
    then return $ Number $ case parseOnly double raw of Right n → n
    else return $ Label raw
  where special c = ord c <= 32 || c == '#' || c == '=' || ord c >= 123

-- Parse everything following a '#' to the eol
comment = takeTill isEndOfLine

-- Whitespace and comments
sep = many' $ space *> pure ()
      <|> char '#' *> comment *> pure ()

value = do
  l ← literal
  _ ← sep
  _ ← char '='
  _ ← sep
  r ← rhs
  return $ Node l r Nothing
  
rhs = block <|> (singleton <$> literal)

block = try (do
  _ ← char '{'
  _ ← sep
  l ← literal
  _ ← sep
  r ← literal
  _ ← sep
  _ ← char '}'
  return $ [Node l (singleton r) Nothing]
            ) <|> (do
  _ ← char '{'
  _ ← sep
  elems ← many' $ do
    l ← literal
    _ ← sep
    _ ← char '='
    _ ← sep
    r ← rhs
    _ ← sep
    return $ Node l r Nothing
  _ ← char '}'
  return elems
                       )

eventId = (,) <$> takeWhile (\c → isAlpha c || c == '_') <*> double
