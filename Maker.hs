{-# LANGUAGE NamedFieldPuns, OverloadedStrings, UnicodeSyntax #-}
module Maker (
  Maker(),runMaker,
  (@@),(@?),(@@@),(@@#),(/@@),(/@#),(<?>),(@~),
  firstChild,secondChild,mapSubForest,filterSubForest,singleChild,
  fetchValue,fetchKey,checkKey,checkKeys,checkValue,checkValues,key,
  Maker.number,leaf,fetchId, fetchBool,position
  ) where

import TreeLike
import Scoped (Error,Label,number,eventId,getPos)

import Text.Parsec(parse)

import Control.Applicative
import Data.Monoid((<>))
import Data.Foldable(find)

import Prelude hiding (concat)

newtype Maker m = Maker (Tree Label -> Either Error m)

runMaker :: Maker m → Tree Label → Either Error m
runMaker (Maker f) = f

instance Functor Maker where
  fmap f (Maker a) = Maker ((f<$>) <$> a)
                  
instance Applicative Maker where
  pure m = Maker $ const (Right m)
  Maker mf <*> Maker ma = Maker $ \t -> case mf t of
    Right f → case ma t of
      Right a → Right $ f a
      Left l → Left l
    Left l → Left l

instance Alternative Maker where
  empty = Maker $ \Node { source } -> Left ("Empty Maker",source)
  Maker mx <|> Maker my = Maker $ \t -> case mx t of
    Left _ → my t
    r → r

infix 0 <?>
(Maker f) <?> msg = Maker $ \t → case f t of
  r@(Right _) → r
  Left (eMsg, source) → Left (msg <> " => " <> eMsg, source)

position = Maker $ Right . getPos

firstChild :: Maker a → Maker a
firstChild (Maker f) = Maker $ \t → case subForest t of
  [] → Left ("No children: "<> rootLabel t,source t)
  (x:_) → f x
secondChild :: Maker a → Maker a
secondChild (Maker f) = Maker $ \t → case subForest t of
  [] → Left ("No children",source t)
  [_] → Left ("Only one child", source t)
  (_:x:_) → f x

singleChild = Maker $ \t → if length (subForest t) == 1
                           then Right ()
                           else Left ("Not one child",source t)

-- m @@ key: Create a new maker that scans through the subForest of the
-- current tree to find a rootLabel matching key. Then apply m to that
-- sub-tree. If no tree is found, the maker fails
infixl 5 @@
(@@) :: Maker a → Label → Maker a
(Maker f) @@ k = Maker $ \t → case find ((==k) . rootLabel) $ subForest t of
  Nothing → Left ("No child: " <> k, source t)
  Just t' → f t'

infixl 5 @~
f @~ k = firstChild f @@ k

-- m @? key: As @@, but if no child is found, return Nothing
infixl 5 @?
(@?) :: Maker a → Label → Maker (Maybe a)
(Maker f) @? k = Maker $ \t → case find ((==k) . rootLabel) $ subForest t of
  Nothing → Right Nothing
  Just t' → Just <$> f t'

fetchKey = Maker $ \t → Right $ rootLabel t
fetchValue = firstChild fetchKey
mapSubForest (Maker f) = Maker $ \t -> mapM f $ subForest t
filterSubForest p (Maker f) = Maker $ f . (\Node { subForest = s, rootLabel, source } -> Node { subForest = filter p s, rootLabel, source })
checkKey k = Maker $ \t -> if k == rootLabel t
                             then Right $ rootLabel t
                             else Left ("Check for "<>k<>" failed. Found: "<>rootLabel t,source t)
checkValue = firstChild . checkKey
checkKeys keys = Maker $ \t → if rootLabel t `elem` keys
                              then Right $ rootLabel t
                              else Left ("Check for keys failed. Found: " <> rootLabel t, source t)
checkValues :: [Label] → Maker Label
checkValues = firstChild . checkKeys
number = Maker $ \t → case parse Scoped.number "" $ rootLabel t of
  Left _ → Left ("Not a number: "<> rootLabel t, source t)
  Right n → Right n

leaf = Maker $ \t → if null $ subForest t
                    then Right $ rootLabel t
                    else Left ("Not a leaf: "<> rootLabel t, source t)

key :: Maker Label
key = Maker $ \ t → Right $ rootLabel t

-- m @@@ key: As @@, but return make a list with every matching subTree
infixl 5 @@@
(@@@) :: Maker a -> Label -> Maker [a]
f @@@ k = filterSubForest ((==k) . rootLabel) (mapSubForest f)

-- m /@@ key: As @@@, but use all subtrees that don't match
infixl 5 /@@
(/@@) :: Maker a → Label → Maker [a]
f /@@ k = filterSubForest ((/=k) . rootLabel) $ mapSubForest f

-- m @@# keys: As @@@, but with several keys
infixl 5 @@#
(@@#) :: Maker a → [Label] → Maker [a]
f @@# keys = filterSubForest ((`elem`keys) . rootLabel) $ mapSubForest f

-- m /@# keys: As /@@, but with several keys
infixl 5 /@#
(/@#) :: Maker a → [Label] → Maker [a]
f /@# keys = filterSubForest (not . (`elem` keys) . rootLabel) $ mapSubForest f



fetchId = Maker $ \t → case runMaker fetchValue t of
  Left e → Left e
  Right k → case parse eventId "" k of
    Left _ → Left ("Not a valid eventId: "<>k,source t)
    Right e → Right e

fetchBool = Maker $ \t → case runMaker fetchValue t of
  Left e → Left e
  Right k → case k of
    "false" → Right False
    "no" → Right False
    "true" → Right True
    "yes" → Right True
    _ → Left (k <> " is not a boolean value.", source t)
