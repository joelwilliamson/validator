-- | Traverse the AST to find any localisation keys
module GatherLocalisations(Localised(localisations)) where

import Scoped(Label)
import Command as Comm(Command(..),Modifier(..))
import Condition(Condition(..),Scope(..),ScopeType(..),Value(..))
import Event as E(Event(..),Option(..))
import Decision(Decision(..))

import Data.Monoid((<>))
import Data.Maybe(fromMaybe)
import Data.Text(Text())

-- | Any type that can have a localisation belongs to this class.
class Localised t where
  localisations :: t → [Label]

instance Localised () where
  localisations _ = mempty

instance Localised Double where
  localisations _ = mempty

instance Localised Text where
  localisations x = [x]

instance (Localised a, Localised b) ⇒ Localised (a,b) where
  localisations (a,b) = localisations a <> localisations b

instance (Localised a, Localised b, Localised c) ⇒ Localised (a,b,c) where
  localisations (a,b,c) = localisations a <> localisations b <> localisations c

instance Localised a ⇒ Localised [a] where
  localisations = mconcat . map localisations

instance Localised a ⇒ Localised (Maybe a) where
  localisations Nothing = mempty
  localisations (Just x) = localisations x

instance (Localised a, Localised b) ⇒ Localised (Either a b) where
  localisations (Left a) = localisations a
  localisations (Right b) = localisations b


instance Localised Command where
  localisations (If conds comms) = localisations conds <> localisations comms
  localisations (BooleanCommand _ _) = mempty
  localisations Break = mempty
  localisations (ClearWealth _) = mempty
  localisations (NumericCommand _ _) = mempty
  localisations (Random _ _ comms) = localisations comms
  localisations (RandomList os) = localisations os
  localisations (Comm.Scoped s) = localisations s
  localisations (SetAllowViceRoyalties _) = mempty
  localisations (SetFlag _ _) = mempty -- Flags never are localised
  localisations (ClrFlag _ _) = mempty
  localisations SpawnUnit {} = mempty
  localisations (StringCommand _ _) = mempty
  localisations VarOpLit {} = mempty
  localisations VarOpVar {} = mempty
  localisations VarOpScope {} = mempty
  localisations (AddTrait _) = mempty
  localisations (RemoveTrait _) = mempty
  localisations (Concrete _ _) = mempty
  localisations CreateCharacter { culture, religion } = localisations culture <> localisations religion
  localisations (ActivateTitle t _) = [t]
  localisations (BestFitCharacterForTitle title perspective _ title') =
    localisations title
    <> localisations perspective
    <> localisations title'
  localisations BuildHolding {}  = mempty
  localisations (ChangeTech tech _) = [tech]
  localisations (CreateTitle _ _ _ _ titleCulture name holder _ base _) =
    localisations titleCulture
    <> localisations name
    <> localisations holder
    <> localisations base
  localisations (Death _ _) = mempty
  localisations (GainSettlementsUnderTitle title enemy) = localisations title <> localisations enemy
  localisations (OpinionModifier mod who _ _) = localisations mod <> localisations who
  localisations (ReligionAuthority (Left _)) = mempty
  localisations (ReligionAuthority (Right mod)) = localisations mod
  localisations (RemoveOpinion mod who _) = localisations mod <> localisations who
  localisations (ScopedModifier name _) = [name]
  localisations (TriggerEvent _ _ tt) = localisations tt
  localisations War { casusBelli } = localisations casusBelli

instance Localised Modifier where
  localisations (Modifier _ _) = mempty

instance Localised Condition where
  localisations _ = mempty

instance Localised c => Localised (Value c) where
  localisations _ = mempty

instance Localised Condition.ScopeType where
  localisations _ = mempty

instance Localised c => Localised (Scope c) where
  localisations (Scope _ _ cont) = localisations cont

instance Localised Event where
  localisations Event { E.title, desc, options, hideWindow } = case hideWindow of
    Just True → mempty
    _ → title' <> desc' <> concat (localisations <$> options)
    where title' = [fromMaybe mempty title]
          desc' = [fromMaybe mempty desc]
            
instance Localised E.Option where
  localisations E.Option { E.name, action } = actions' <> name'
    where actions' = localisations action
          name' = [fromMaybe mempty name]

instance Localised Decision where
  localisations Decision {..} = [name,name<>"_desc"]
