-- | Traverse the entire AST to find any arguments to string-like commands or conditions
module GatherStrings(GatherStrings(gatherStrings)) where

import Scoped(Label)
import Command as Comm(Command(..),Modifier(..),stringyCommands)
import Condition(Clause(..),Condition(..),Predicate(..),Scope(..),ScopeType(..),Value(..))
import Event as E(Event(..),Option(..))
import Decision(Decision(..))

import Data.Text(Text)
import Data.Monoid((<>))
import Data.Maybe(fromMaybe)

-- | Any type that can contain a string-like key belongs to this class.
class GatherStrings t where
  gatherStrings :: t -> [Label]
  
instance GatherStrings () where
  gatherStrings _ = []

instance GatherStrings Double where
  gatherStrings _ = []

instance GatherStrings Text where
  gatherStrings x = [x]

instance (GatherStrings a, GatherStrings b) => GatherStrings (a,b) where
  gatherStrings (a,b) = gatherStrings a <> gatherStrings b

instance (GatherStrings a, GatherStrings b, GatherStrings c) => GatherStrings (a,b,c) where
  gatherStrings (a,b,c) = gatherStrings a <> gatherStrings b <> gatherStrings c

instance GatherStrings a ⇒ GatherStrings [a] where
  gatherStrings = concatMap gatherStrings

instance GatherStrings a ⇒ GatherStrings (Maybe a) where
  gatherStrings = concatMap gatherStrings

instance GatherStrings Command where
  gatherStrings (If conds comms) = gatherStrings conds <> gatherStrings comms
  gatherStrings Break = []
  gatherStrings (Random _ mods comms) = gatherStrings mods <> gatherStrings comms
  gatherStrings (RandomList os) = gatherStrings os
  gatherStrings (Comm.Scoped s) = gatherStrings s
  gatherStrings (SetFlag _ _) = [] -- Flags never are stringy
  gatherStrings (ClrFlag _ _) = []
  gatherStrings SpawnUnit { earmark } = gatherStrings earmark
  gatherStrings (VarOpLit t _ _) = [t]
  gatherStrings (VarOpVar t _ t') = [t,t']
  gatherStrings (VarOpScope t _ s) = [t] <> gatherStrings s
  gatherStrings (Concrete t v) = if t `elem` stringyCommands
                                 then gatherStrings v
                                 else []
  gatherStrings (AddTrait _) = []
  gatherStrings (RemoveTrait _) = []
  gatherStrings (ActivateTitle title _) = [title]
  gatherStrings (BestFitCharacterForTitle title perspective _ title') =
    gatherStrings title
    <> gatherStrings perspective
    <> gatherStrings title'
  gatherStrings (BuildHolding prov holding holder) = prov : holding : gatherStrings holder
  gatherStrings (ChangeTech tech _) = [tech]
  gatherStrings (CreateCharacter { name, hasNickName, employer, religion, culture, dynasty, flag, father, mother, race }) =
    gatherStrings name
    <> gatherStrings hasNickName
    <> gatherStrings employer
    <> gatherStrings religion
    <> gatherStrings culture
    <> gatherStrings dynasty
    <> gatherStrings flag
    <> gatherStrings father
    <> gatherStrings mother
    <> gatherStrings race
  gatherStrings (CreateTitle tier _ _ _ titleCulture name holder _ base _) =
    [tier]
    <> gatherStrings titleCulture
    <> gatherStrings name
    <> gatherStrings holder
    <> gatherStrings base
  gatherStrings (Death reason killer) = gatherStrings reason <> gatherStrings killer
  gatherStrings (GainSettlementsUnderTitle title enemy) = gatherStrings title <> gatherStrings enemy
  gatherStrings (OpinionModifier mod who _) = gatherStrings mod <> gatherStrings who
  gatherStrings (TriggerEvent _ _ _) = []

instance GatherStrings Modifier where
  gatherStrings (Modifier _ conds) = gatherStrings conds

instance GatherStrings Condition where
  gatherStrings (Condition (Predicate _) v) = gatherStrings v
  gatherStrings (Condition.Scoped s) = gatherStrings s
  gatherStrings (VariableCheck _ _) = []
  gatherStrings (Trait _) = []
  gatherStrings (Or cs) = gatherStrings cs
  gatherStrings (And cs) = gatherStrings cs
  gatherStrings (Not c) = gatherStrings c

instance GatherStrings c => GatherStrings (Value c) where
  gatherStrings (BooleanValue _) = []
  gatherStrings (NumValue _) = []
  gatherStrings (ScopedValue s) = gatherStrings s
  gatherStrings (Clause c) = gatherStrings c
  gatherStrings (Id t) = [t]

instance GatherStrings Condition.ScopeType where
  gatherStrings (IdScope i) = [i]
  gatherStrings _ = []

instance GatherStrings c => GatherStrings (Clause c) where
  gatherStrings _ = []
  
instance GatherStrings c => GatherStrings (Scope c) where
  gatherStrings (Scope ty limit cont) = gatherStrings ty
                                        <> gatherStrings limit
                                        <> gatherStrings cont

instance GatherStrings Event where
  gatherStrings Event { trigger, immediate, options} = trigger' <> immediate' <> options'
    where trigger' = [] `fromMaybe` (gatherStrings <$> trigger)
          immediate' = [] `fromMaybe` (gatherStrings <$> immediate)
          options' = gatherStrings options
            
instance GatherStrings E.Option where
  gatherStrings E.Option { optionTrigger, action, aiChance } = triggers' <> actions' <> chance'
    where triggers' = gatherStrings optionTrigger
          actions' = gatherStrings action
          chance' = [] `fromMaybe` (gatherStrings <$> aiChance)
          
instance GatherStrings Decision where
  gatherStrings Decision {..} = gatherStrings potential
                                <> gatherStrings allow
                                <> gatherStrings effect
                                <> gatherStrings aiWillDo
