-- | A @`Maker` a@ is wrapper around a function from @`Tree`@'s to @a@s. They are
-- built up using various combinators, then invoked with @`runMaker`@.
module Maker (
  Maker(),runMaker,
  (@@),(@?),(@@@),(@@#),(/@@),(/@#),(<?>),
  (~@),(~?),
  firstChild,secondChild,mapSubForest,filterSubForest,singleChild,
  checkKey,checkKeys,excludeKeys,checkValue,checkValues,key,
  fetchString,label,except,optional,
  Maker.number,fetchId, fetchBool,position
  ) where

import Tree(Tree(..))
import Scoped (Error,Label,Atom(..),eventId)
import qualified Data.Text as T(pack,Text)

import Text.Parsec(parse)

import Control.Applicative(Alternative(empty),(<|>))
import Data.Monoid((<>))
import Data.Foldable(find)

import Prelude hiding (concat,lookup)

show' :: Show a ⇒ a → T.Text
show' = T.pack . show

-- | Describes how to make an @a@ from a @`Tree` Atom@.
-- If there is an error, the integer is flags how likely it is that the error
-- message will be useful. For instance, if a key doesn't match, we are likely
-- to backtrack out of a wrong branch of an alternative, but if the structure
-- is wrong, this is probably an error.
newtype Maker a = Maker (Tree Atom -> Either (Error,Reason) a)

data Reason = KeyNotFound
            | BadConversion
            | MissingChild
            deriving (Eq,Ord,Show)

-- | @runMaker maker t@ uses the description provided by @maker@ to construct an
-- @a@.
runMaker :: Maker a → Tree Atom → Either Error a
runMaker (Maker f) = \t -> case f t of
  Left (e,_) -> Left e
  Right r -> Right r

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
  empty = Maker $ \Node { source } -> Left (("Empty Maker",source), MissingChild)
  Maker mx <|> Maker my = Maker $ \t -> case (mx t, my t) of
    (x@(Left (_,ix)), y@(Left (_,iy))) → if ix > iy then x else y 
    (Right x, _) -> Right x
    (_, Right y) -> Right y

optional :: Maker a -> Maker (Maybe a)
optional (Maker f) = Maker $ \t -> case f t of
  Right r -> Right $ Just r
  Left _ -> Right Nothing

instance Monad Maker where
  return x = Maker $ \_ → Right x
  (Maker f1) >>= f = Maker $ \t → case f1 t of
    Left l → Left l
    Right a → case f a of
      Maker f2 -> f2 t

-- | @maker \<?\> err@ behaves the same as @maker@, but if @maker@ fails, the error
-- message is prefixed by "@err@ =>"
infix 0 <?>
(Maker f) <?> msg = Maker $ \t → case f t of
  r@(Right _) → r
  Left ((eMsg, source),prio) → Left ((msg <> " => " <> eMsg, source),prio)

-- | Get the source position this tree was created from.
position = Maker $ Right . source

-- | @firstChild m@ applies @m@ to the first child node.
firstChild :: Maker a → Maker a
firstChild (Maker f) = Maker $ \t → case subForest t of
  [] → Left (("No children: "<> show' (rootLabel t),source t), MissingChild)
  (x:_) → f x
-- | @secondChild m@ applies @m@ to the second child node.
secondChild :: Maker a → Maker a
secondChild (Maker f) = Maker $ \t → case subForest t of
  [] → Left (("No children",source t), MissingChild)
  [_] → Left (("Only one child", source t), MissingChild)
  (_:x:_) → f x

-- | @mapSubForest m@ applies @m@ to every child node.
mapSubForest (Maker f) = Maker $ \t -> mapM f $ subForest t

-- | @filterSubForest p m@ removes any subtree that @o@ doesn't match, then
-- applies @m@ to the entire result.
filterSubForest p (Maker f) = Maker $ f . (\Node { subForest = s,
                                                   rootLabel,
                                                   source } ->
                                           Node { subForest = filter p s,
                                                  rootLabel,
                                                  source })

-- | Fail unless there is exactly one child.
singleChild = Maker $ \t → if length (subForest t) == 1
                           then Right ()
                           else Left (("Not one child",source t), MissingChild)

-- | @m \@\@ key@ makes @m@ by using only a subtree with rootLabel @key@. If no such
-- subtree exists (or if @m@ fails) the entire @`Maker`@ fails.
infixl 5 @@
(@@) :: Maker a → Label → Maker a
(Maker f) @@ k = Maker $ \t → case find ((==Label k) . rootLabel) $ subForest t of
  Nothing → Left (("No child: " <> k <> " in block: " <> T.pack (show t), source t)
                  , MissingChild)
  Just t' → f t'

-- | @m \@? key@: As @\@\@@, but if no child is found, return @Nothing@.
infixl 5 @?
(@?) :: Maker a → Label → Maker (Maybe a)
(Maker f) @? k = Maker $ \t → case find ((==Label k) . rootLabel) $ subForest t of
  Nothing → Right Nothing
  Just t' → Just <$> f t'

-- | @m \~\@ key@ makes @m@ by using only the first child of the subtree with
-- rootLabel @key@. If no such subtree exists (or if @m@ fails) the entire
-- @`Maker`@ fails.
infixl 5 ~@
(~@) :: Maker a → Label → Maker a
m ~@ k = firstChild m @@ k

-- | @m \~? key@: As @\~\@@, but if no child is found, return @Nothing@.
infixl 5 ~?
(~?) :: Maker a → Label → Maker (Maybe a)
m ~? k = firstChild m @? k

-- | @checkKey key@ succeeds if the root label is key
checkKey k = Maker $ \t -> case rootLabel t of
  Label l → if l == k
            then Right l
            else Left (("Check for "<>show' k<>" failed. Found: "<>show' (rootLabel t)
                        ,source t)
                       , KeyNotFound)
  Number _ → Left (("Check for key "<>show' k<>" failed. Found number",source t)
                   , KeyNotFound)

-- | @checkKeys keys@ succeeds if the root label is an element of keys.
checkKeys keys = Maker $ \t → if rootLabel t `elem` map Label keys
                              then Right $ rootLabel t
                              else Left (("Check for keys failed. Found: " <> show' (rootLabel t), source t)
                                         , KeyNotFound)

-- | @except m1 m2@ is equivalent to `m2` if `m1` fails, and fails if `m1` succeeds
except (Maker f1) (Maker f2) = Maker $ \t -> case f1 t of
  Right _ -> Left (("Excepted maker succeeded: " <> show' (rootLabel t), source t)
                   , KeyNotFound)
  Left _ -> f2 t

-- | @excludeKeys keys@ succeeds with the root label provided the label is not in `keys`.
excludeKeys keys = Maker $ \t → if not $ rootLabel t `elem` map Label keys
                                then Right $ rootLabel t
                                else Left (("Root label excluded: " <> show' (rootLabel t), source t)
                                           , KeyNotFound)

-- | @checkValue v@ succeeds if the root label of the first child is @v@.
checkValue = firstChild . checkKey
-- | @checkValue vs@ succeeds if the root label of the first child is in @vs@.
checkValues = firstChild . checkKeys

-- | Succeed with the root label of the tree
key :: Maker Atom
key = Maker $ \ t → Right $ rootLabel t

-- | If the root label is a number, succeed with it.
number = Maker $ \t → case rootLabel t of
  Scoped.Label l → Left (("Not a number: "<> show' l, source t), BadConversion)
  Scoped.Number n → Right n

-- | Get the label of the tree
label :: Maker Atom → Maker Label
label (Maker f) = Maker $ \t → case f t of
  Left l → Left l
  Right (Label l) → Right l
  Right (Number n) → Left (("Encountered number when expecting a label: " <> show' n, source t), BadConversion)

-- | @m \@\@\@ key@: As @\@\@@, but return make a list with every matching subTree
infixl 5 @@@
(@@@) :: Maker a -> Label -> Maker [a]
f @@@ k = filterSubForest ((== Label k) . rootLabel) (mapSubForest f)

-- | @m /\@\@ key@: As @\@\@\@@, but use all subtrees that don't match
infixl 5 /@@
(/@@) :: Maker a → Label → Maker [a]
f /@@ k = filterSubForest ((/= Label k) . rootLabel) $ mapSubForest f

-- | @m \@\@# keys@: As @@@, but use any subtree whose label is in @keys@.
infixl 5 @@#
(@@#) :: Maker a → [Label] → Maker [a]
f @@# keys = filterSubForest ((`elem` (Label <$> keys)) . rootLabel) $ mapSubForest f

-- | m \/\@# keys: As /\@\@, but with any subtree whose label is not in @keys@.
infixl 5 /@#
(/@#) :: Maker a → [Label] → Maker [a]
f /@# keys = filterSubForest (not . (`elem` (Label <$> keys)) . rootLabel) $ mapSubForest f

-- | Get the root label of the single child. If the label is a number, fail.
fetchString = Maker $ \t → case subForest t of
  [] → Left (("Can't take a value at " <> show' (rootLabel t),source t), BadConversion)
  [Node { rootLabel = Number n}] →
    Left (("Expected string for value of "<> show' (rootLabel t) <>", got: "<> show' n
           , source t)
          , BadConversion)
  [Node { rootLabel = Label l}] → Right l
  _ → Left (("Expected a string for value of "<> show' (rootLabel t) <> ", got a block."
             , source t)
            , BadConversion)

-- | Get an event id from the label of the single child. This can either be of
-- the form @Number id@ or @Label $ namespace <> "." <> number@.
fetchId = Maker $ \t → case firstChild key of
  Maker f -> case f t of
    Left e → Left e
    Right (Label k) → case parse eventId "fetchID parse" k of
      Left _ → Left (("Not a valid eventId: "<>k, source t), BadConversion)
      Right (t,Number n) → Right (t,n)
      Right (n,Label x) → Left (("Ill-formed eventId: "<>n<>"."<>x, source t)
                                , BadConversion)
    Right (Number n) → Right ("",n)

-- | Get a boolean from the label of the single child. The label must be one of
-- "yes", "no", "true", "false".
fetchBool = Maker $ \t → case firstChild key of
  Maker f -> case f t of
    Left e → Left e
    Right (Label k) → case k of
      "false" → Right False
      "no" → Right False
      "true" → Right True
      "yes" → Right True
      _ → Left ((k <> " is not a boolean value.", source t), BadConversion)
    Right (Number n) → Left (("Expected boolean, got: " <> show' n, source t)
                             , BadConversion)
