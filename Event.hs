{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Event where

import Scoped(EventId,Atom(..),Label,Error,lookup)
import AttoScoped as A(sep,value)
import Condition(Condition,condition)
import TreeLike(TreeLike(..),Tree(..))
import Maker(Maker,(@@),(@?),(@@@),(/@@),(/@#),(<?>),(@@#)
           ,position,mapSubForest,fetchBool,fetchId,firstChild,number,fetchString,key)
import Command(Command,command)

import Data.Attoparsec.Text(many')
import Text.Parsec hiding (label,(<?>),option)
import Data.Text
import qualified Data.ByteString as BS
import Control.Monad.Writer
import qualified Data.List as L

import Prelude hiding (id,lines,lookup,putStrLn,unlines)


render Node {rootLabel = l, subForest =  [Node { rootLabel = v, subForest = [] }]} = l <> " = " <> v
render Node { rootLabel = l, subForest = v } = l <> " = {\n" <> unlines (indent <$> L.concat (lines <$> render <$> v)) <> "}"
    where indent = ("\t"<>)

  
--data LocalisationKey =deriving (Eq,Show)
data EventFlag = EventFlag deriving (Eq,Show)
instance TreeLike EventFlag where
  toTree EventFlag = error "EventFlag not implemented"

type DisplayText = Text
-- Path to the gfx resource
newtype Gfx = Gfx Text deriving (Eq,Ord,Show)
instance TreeLike Gfx where
  toTree (Gfx s) = Node s [] Nothing
newtype Sfx = Sfx Text deriving (Eq,Ord,Show)
instance TreeLike Sfx where
  toTree (Sfx s) = Node s [] Nothing


data Event = Event {
  id :: EventId,
  title :: Maybe DisplayText,
  desc :: Maybe DisplayText,
  picture :: Maybe Gfx,
  border :: Maybe Gfx,
  major :: Maybe Bool,
  isTriggeredOnly :: Maybe Bool,
  hideFrom :: Maybe Bool,
  hideNew :: Maybe Bool,
  hideWindow :: Maybe Bool,
  showRoot :: Maybe Bool,
  showFrom :: Maybe Bool,
  showFromFrom :: Maybe Bool,
  showFromFromFrom :: Maybe Bool,
  sound :: Maybe Sfx,
  notification :: Maybe Bool,
  trigger :: Maybe [Condition],
  immediate :: Maybe [Command],
  options :: [Option],
  source :: Maybe SourcePos
  }
           deriving (Eq,Ord,Show)

data Option = Option {
  name :: Maybe Label,
  optionTrigger :: [Condition],
  action :: [Command],
  aiChance :: Maybe (Double,[(Double,[Condition])])
  } deriving (Eq,Ord,Show)

option :: Maker Option
option = Option <$> (getLabel <$> firstChild key) @? "name"
         <*> (Prelude.concat <$> mapSubForest condition @? "trigger" :: Maker [Condition])
         <*> command /@# ["trigger","name","ai_chance"]
         <*> (((,) <$> firstChild number @@ "factor" <*> modifier @@@ "modifier") @? "ai_chance" <?> "ai_chance")
         <?> "Option"
  where modifier = (,) <$> firstChild number @@ "factor" <*> condition /@@ "factor"
  
--eventFile :: Parsec Text u1 ([Tree Atom],[Tree Atom])
eventFile = do
  _ ← A.sep
  namespacesEvents ← many' A.value
  let namespaces = L.filter ((==Label "namespace") . rootLabel) namespacesEvents
  let events = L.filter ((/=Label "namespace") . rootLabel) namespacesEvents
  return (namespaces,events)

checkBool :: Text → Tree Text → Either Error (Maybe Bool)
checkBool key t = case lookup key t of
  Just "true" → Right $ Just True
  Just "yes" → Right $ Just True
  Just "false" → Right $ Just False
  Just "no" → Right $ Just False
  Just v → Left (key <> " has the non-boolean value: " <> v, TreeLike.source t)
  Nothing → Right Nothing

event :: Maker Event
event = Event
        <$> fetchId @@ "id"
        <*> (getLabel <$> firstChild key) @? "title"
        <*> (getLabel <$> firstChild key) @? "desc"
        <*> ((Gfx <$>) <$> fetchString @? "picture")
        <*> ((Gfx <$>) <$> fetchString @? "border")
        <*> fetchBool @? "major"
        <*> fetchBool @? "is_triggered_only"
        <*> fetchBool @? "hide_from"
        <*> fetchBool @? "hide_new"
        <*> fetchBool @? "hide_window"
        <*> fetchBool @? "show_root"
        <*> fetchBool @? "show_from"
        <*> fetchBool @? "show_from_from"
        <*> fetchBool @? "show_from_from_from"
        <*> ((Sfx <$>) <$> fetchString @? "sound")
        <*> fetchBool @? "notification"
        <*> mapSubForest condition @? "trigger"
        <*> mapSubForest command @? "immediate"
        <*> option @@@ "option"
        <*> position

getLabel (Label l ) = l
getLabel (Number _) = error "Tried to getLabel on a number"
        
renderId (name,n) = name ++ show n
stripBoM input = if BS.length input < 3
                 then input
                 else case BS.take 3 input of
                   "\xef\xbb\xbf" → BS.drop 3 input
                   _ → input
