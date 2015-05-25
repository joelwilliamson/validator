{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Event where

import Scoped(EventId,Atom(..),lookup,Namespace(),Label(),parseValue,Error(),quickParse,getValue,sep)
import Condition(Condition,condition)
import TreeLike(TreeLike(..),Tree(..))
import Maker(Maker,(@@),(@?),(@@@),(/@@),(/@#),(<?>)
           ,position,runMaker,mapSubForest,fetchBool,fetchValue,fetchId,firstChild,number,fetchString,key)
import Command(Command,command)

import Text.Parsec hiding (label,(<?>),option)
import Data.Text
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Enc
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
{-
instance TreeLike Event where
  toTree e = Node "character_event" (execWriter $ do
                                        tell [Node { rootLabel = "id"
                                                   , TreeLike.source = Nothing
                                                   , subForest = [Node { rootLabel = fst (id e)
                                                                                     <> "."
                                                                                     <> pack (show $ snd $ id e)
                                                                       , subForest = []
                                                                       , TreeLike.source = Nothing}]
                                                   }]
                                        tell $ field title "title"
                                        tell $ field desc "desc"
                                        tell $ field picture "picture"
                                        tell $ field border "border"
                                        tell $ field major "major"
                                        tell $ field isTriggeredOnly "is_triggered_only"
                                        tell $ field hideFrom "hide_from"
                                        tell $ field hideNew "hide_new"
                                        tell $ field hideWindow "hide_window"
                                        tell $ field showRoot "show_root"
                                        tell $ field showFrom "show_from"
                                        tell $ field showFromFrom "show_from_from"
                                        tell $ field showFromFromFrom "show_from_from_from"
                                        tell $ field sound "sound"
                                        tell $ field notification "notification"
                                        tell cond ) Nothing
    where field acc name = case acc e of
            Just v -> [Node name [toTree v] Nothing]
            Nothing -> []
          cond = case trigger e of
            Just [] -> []
            Just ts -> [Node "trigger" (toTree <$> ts) Nothing]
            Nothing → []
 -} 
data Option = Option {
  name :: Maybe Label,
  optionTrigger :: [Condition],
  action :: [Command],
  aiChance :: Maybe (Double,[(Double,[Condition])])
  } deriving (Eq,Ord,Show)

option :: Maker Option
option = Option <$> (getLabel <$> firstChild key) @? "name"
         <*> condition @@@ "trigger"
         <*> command /@# ["trigger","name","ai_chance"]
         <*> (((,) <$> firstChild number @@ "factor" <*> modifier @@@ "modifier") @? "ai_chance" <?> "ai_chance")
         <?> "Option"
  where modifier = (,) <$> firstChild number @@ "factor" <*> condition /@@ "factor"
  
eventFile :: Parsec Text u1 ([Tree Atom],[Tree Atom])
eventFile = do
  sep
  namespacesEvents ← many parseValue
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
        
renderId (name,n) = name ++ show n
stripBoM input = if BS.length input < 3
                 then input
                 else case BS.take 3 input of
                   "\xef\xbb\xbf" → BS.drop 3 input
                   _ → input
eventMaker f = do
  contents ← BS.readFile f
  let (namespace,events) = quickParse eventFile $ Enc.decodeUtf8 $ stripBoM contents
  return (getValue <$> namespace, runMaker event <$> events)
