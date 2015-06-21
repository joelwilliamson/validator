{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Event
       (
         Event(..)
       , Option(..)
       , event
       , eventOrNamespace
       )where

import Scoped(EventId,Atom(..),Label)
import Condition(Condition,condition)
import Maker(Maker,(@@),(@?),(@@@),(/@@),(/@#),(<?>)
           ,checkKey,position,mapSubForest,fetchBool
           ,fetchId,firstChild,number,fetchString,key)
import Command(Command,command)

import Control.Applicative((<|>))
import Text.Parsec.Pos(SourcePos)
import Data.Text

import Prelude hiding (id,lines,lookup,putStrLn,unlines)

data EventFlag = EventFlag deriving (Eq,Show)

type DisplayText = Text
-- Path to the gfx resource
newtype Gfx = Gfx Text deriving (Eq,Ord,Show)
newtype Sfx = Sfx Text deriving (Eq,Ord,Show)

data EventType = CharacterEvent
               | DiploResponseEvent
               | LetterEvent
               | LongCharacterEvent
               | NarrativeEvent
               | ProvinceEvent
               | UnitEvent
               deriving (Eq,Ord,Show)
eventTyp = CharacterEvent <$ checkKey "character_event"
           <|> DiploResponseEvent <$ checkKey "diploresponse_event"
           <|> LetterEvent <$ checkKey "letter_event"
           <|> LongCharacterEvent <$ checkKey "long_character_event"
           <|> NarrativeEvent <$ checkKey "narrative_event"
           <|> ProvinceEvent <$ checkKey "province_event"
           <|> UnitEvent <$ checkKey "unit_event"

data Event = Event {
  eventType :: EventType,
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
  
event :: Maker Event
event = Event
        <$> eventTyp
        <*> fetchId @@ "id"
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

namespace :: Maker Label
namespace = checkKey "namespace" *> (getLabel <$> firstChild key)

eventOrNamespace :: Maker (Either Event Label)
eventOrNamespace =  Left <$> event <|> Right <$> namespace

getLabel (Label l ) = l
getLabel (Number _) = error "Tried to getLabel on a number"
