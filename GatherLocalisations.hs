{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- Traverse the AST to find any localisation keys

module GatherLocalisations where

import Scoped(Label)
import Command as Comm(Command(..),Modifier(..))
import Condition(Clause(..),Condition(..),Scope(..),ScopeType(..),Value(..))
import Event as E(Event(..),Option(..))
import Decision(Decision(..))

import Data.Monoid((<>))
import Data.Maybe(fromMaybe)

class Localised t where
  localisations :: t → [Label]

instance Localised () where
  localisations _ = []

instance Localised Double where
  localisations _ = []

instance (Localised a, Localised b) ⇒ Localised (a,b) where
  localisations (a,b) = localisations a <> localisations b

instance (Localised a, Localised b, Localised c) ⇒ Localised (a,b,c) where
  localisations (a,b,c) = localisations a <> localisations b <> localisations c

instance Localised a ⇒ Localised [a] where
  localisations = mconcat . map localisations


instance Localised Command where
  localisations (If conds comms) = localisations conds <> localisations comms
  localisations Break = []
  localisations (Random _ _ comms) = localisations comms
  localisations (RandomList os) = localisations os
  localisations (Comm.Scoped s) = localisations s
  localisations (SetFlag _ _) = [] -- Flags never are localised
  localisations (ClrFlag _ _) = []
  localisations SpawnUnit {} = []
  localisations VarOpLit {} = []
  localisations VarOpVar {} = []
  localisations VarOpScope {} = []
  localisations (Concrete _ _) = []

instance Localised Modifier where
  localisations (Modifier _ _) = []

instance Localised Condition where
  localisations _ = []

instance Localised c => Localised (Value c) where
  localisations _ = []

instance Localised Condition.ScopeType where
  localisations _ = []

instance Localised c => Localised (Clause c) where
  localisations _ = []
  
instance Localised c => Localised (Scope c) where
  localisations (Scope _ _ cont) = localisations cont

instance Localised Event where
  localisations Event { E.title, desc, options, hideWindow } = case hideWindow of
    Just True → []
    _ → title' <> desc' <> concat (localisations <$> options)
    where title' = [] `fromMaybe` ((:[]) <$> title)
          desc' = [] `fromMaybe` ((:[]) <$> desc)
            
instance Localised E.Option where
  localisations E.Option { E.name, action } = actions' <> name'
    where actions' = localisations action
          name' = [] `fromMaybe` ((:[]) <$> name)

instance Localised Decision where
  localisations Decision {..} = [name,name<>"_desc"]
