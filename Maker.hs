{-# LANGUAGE NamedFieldPuns, OverloadedStrings, UnicodeSyntax #-}
module Maker (
  Maker(),runMaker,
  (@@),(@?),(@@@),(@@#),(/@@),(/@#),(<?>),(@~),
  firstChild,secondChild,mapSubForest,filterSubForest,singleChild,
  fetchValue,fetchKey,checkKey,checkKeys,checkValue,checkValues,key,
  fetchString,label,fetchLabel,
  Maker.number,leaf,fetchId, fetchBool,position
  ) where

import TreeLike
import Scoped (Error,Label,Atom(..),eventId,getPos)
import qualified Data.Text as T(pack,Text)

import Text.Parsec(parse)

import Control.Applicative
import Data.Monoid((<>))
import Data.Foldable(find)

import Prelude hiding (concat)

show' :: Show a ⇒ a → T.Text
show' = T.pack . show

newtype Maker m = Maker (Tree Atom -> Either Error m)

runMaker :: Maker m → Tree Atom → Either Error m
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

instance Monad Maker where
  return x = Maker $ \_ → Right x
  (Maker f1) >>= f = Maker $ \t → case f1 t of
    Left l → Left l
    Right a → runMaker (f a) t

infix 0 <?>
(Maker f) <?> msg = Maker $ \t → case f t of
  r@(Right _) → r
  Left (eMsg, source) → Left (msg <> " => " <> eMsg, source)

position = Maker $ Right . getPos

firstChild :: Maker a → Maker a
firstChild (Maker f) = Maker $ \t → case subForest t of
  [] → Left ("No children: "<> show' (rootLabel t),source t)
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
(Maker f) @@ k = Maker $ \t → case find ((==Label k) . rootLabel) $ subForest t of
  Nothing → Left ("No child: " <> k <> " in block: " <> T.pack (show t), source t)
  Just t' → f t'

infixl 5 @~
f @~ k = firstChild f @@ k

-- m @? key: As @@, but if no child is found, return Nothing
infixl 5 @?
(@?) :: Maker a → Label → Maker (Maybe a)
(Maker f) @? k = Maker $ \t → case find ((==Label k) . rootLabel) $ subForest t of
  Nothing → Right Nothing
  Just t' → Just <$> f t'

fetchKey = Maker $ \t → Right $ rootLabel t
fetchLabel = Maker $ \t → case rootLabel t of
  Number n → Right $ show' n
  Label l → Right l
fetchValue = firstChild fetchKey
fetchString = Maker $ \t → case subForest t of
  [] → Left ("Can't take a value at " <> show' (rootLabel t),source t)
  [Node { rootLabel = Number n}] → Left ("Expected string for value of "<> show' (rootLabel t) <>", got: "<> show' n,source t)
  [Node { rootLabel = Label l}] → Right l
  _ → Left ("Expected a string for value of "<> show' (rootLabel t) <> ", got a block.", source t)
mapSubForest (Maker f) = Maker $ \t -> mapM f $ subForest t
filterSubForest p (Maker f) = Maker $ f . (\Node { subForest = s, rootLabel, source } -> Node { subForest = filter p s, rootLabel, source })
checkKey k = Maker $ \t -> case rootLabel t of
  Label l → if l == k
            then Right l
            else Left ("Check for "<>show' k<>" failed. Found: "<>show' (rootLabel t),source t)
  Number _ → Left ("Check for key "<>show' k<>" failed. Found number",source t)
checkValue = firstChild . checkKey
checkKeys keys = Maker $ \t → if rootLabel t `elem` map Label keys
                              then Right $ rootLabel t
                              else Left ("Check for keys failed. Found: " <> show' (rootLabel t), source t)
checkValues :: [Label] → Maker Atom
checkValues = firstChild . checkKeys
number = Maker $ \t → case rootLabel t of
  Scoped.Label l → Left ("Not a number: "<> show' l, source t)
  Scoped.Number n → Right n

leaf = Maker $ \t → if null $ subForest t
                    then case rootLabel t of
                      Number n → Right $ show' n
                      Label l → Right l
                    else Left ("Not a leaf: "<> show' (rootLabel t), source t)

label :: Maker Atom → Maker Label
label (Maker f) = Maker $ \t → case f t of
  Left l → Left l
  Right (Label l) → Right l
  Right (Number n) → error $ "Encountered number " <> show n
key :: Maker Atom
key = Maker $ \ t → Right $ rootLabel t

-- m @@@ key: As @@, but return make a list with every matching subTree
infixl 5 @@@
(@@@) :: Maker a -> Label -> Maker [a]
f @@@ k = filterSubForest ((== Label k) . rootLabel) (mapSubForest f)

-- m /@@ key: As @@@, but use all subtrees that don't match
infixl 5 /@@
(/@@) :: Maker a → Label → Maker [a]
f /@@ k = filterSubForest ((/= Label k) . rootLabel) $ mapSubForest f

-- m @@# keys: As @@@, but with several keys
infixl 5 @@#
(@@#) :: Maker a → [Label] → Maker [a]
f @@# keys = filterSubForest ((`elem` (Label <$> keys)) . rootLabel) $ mapSubForest f

-- m /@# keys: As /@@, but with several keys
infixl 5 /@#
(/@#) :: Maker a → [Label] → Maker [a]
f /@# keys = filterSubForest (not . (`elem` (Label <$> keys)) . rootLabel) $ mapSubForest f



fetchId = Maker $ \t → case runMaker (firstChild fetchKey) t of
  Left e → Left e
  Right (Label k) → case parse eventId "fetchID parse" k of
    Left _ → Left ("Not a valid eventId: "<>k,source t)
    Right (t,Number n) → Right (t,n)
    Right (n,Label x) → Left ("Ill-formed eventId: "<>n<>"."<>x,source t)
  Right (Number n) → Right ("",n)

fetchBool = Maker $ \t → case runMaker (firstChild fetchKey) t of
  Left e → Left e
  Right (Label k) → case k of
    "false" → Right False
    "no" → Right False
    "true" → Right True
    "yes" → Right True
    _ → Left (k <> " is not a boolean value.", source t)
  Right (Number n) → Left ("Expected boolean, got: " <> show' n, source t)
