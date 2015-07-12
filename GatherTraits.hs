-- | Traverse the AST to find any traits referenced
module GatherTraits(GatherTraits(traits)) where

import Scoped(Label)
import Command as Comm(Command(ActivateTitle,
                               AddTrait,RemoveTrait,
                               Random,RandomList,
                               SetFlag,ClrFlag,
                               SpawnUnit,
                               VarOpLit,VarOpVar,VarOpScope,
                               Concrete,
                               Scoped,
                               Break,If,
                               BestFitCharacterForTitle,
                               BuildHolding,
                               ChangeTech,
                               CreateTitle,
                               Death,
                               GainSettlementsUnderTitle,
                               OpinionModifier,
                               ReligionAuthority,
                               RemoveOpinion,
                               ScopedModifier,
                               TriggerEvent,
                               War),
                       Modifier(..))
import qualified Command (Command(CreateCharacter),traits)
import qualified Condition as Cond(Condition(..),Scope(..),ScopeType(..),Value(..))
import Event as E(Event(..),Option(..))
import Decision(Decision(..))

import Data.Monoid((<>))

-- | Any type that can contain a reference to a trait should belong to this class.
class GatherTraits t where
  traits :: t → [Label]

instance GatherTraits () where
  traits _ = []

instance GatherTraits Double where
  traits _ = []

instance (GatherTraits a, GatherTraits b) ⇒ GatherTraits (a,b) where
  traits (a,b) = traits a <> traits b

instance (GatherTraits a, GatherTraits b, GatherTraits c) ⇒ GatherTraits (a,b,c) where
  traits (a,b,c) = traits a <> traits b <> traits c

instance GatherTraits a ⇒ GatherTraits [a] where
  traits = mconcat . map traits

instance GatherTraits a ⇒ GatherTraits (Maybe a) where
  traits Nothing = []
  traits (Just a) = traits a

instance (GatherTraits a, GatherTraits b) ⇒ GatherTraits (Either a b) where
  traits (Left a) = traits a
  traits (Right b) = traits b

instance GatherTraits Command where
  traits (AddTrait t) = [t]
  traits (RemoveTrait t) = [t]
  traits (If conds comms) = traits conds <> traits comms
  traits Break = []
  traits (Random _ _ comms) = traits comms
  traits (RandomList os) = traits os
  traits (Comm.Scoped s) = traits s
  traits (SetFlag _ _) = [] -- Flags never are localised
  traits (ClrFlag _ _) = []
  traits SpawnUnit {} = []
  traits VarOpLit {} = []
  traits VarOpVar {} = []
  traits VarOpScope {} = []
  traits (Concrete _ _) = []
  traits (Command.CreateCharacter { traits = t }) = t
  traits (ActivateTitle t _) = [t]
  traits (BestFitCharacterForTitle title perspective _ title') =
    traits title
    <> traits perspective
    <> traits title'
  traits BuildHolding {} = mempty
  traits (ChangeTech _ _) = mempty
  traits TriggerEvent {} = mempty
  traits (CreateTitle _ _ _ _ titleCulture _ holder _ _ _) =
    traits titleCulture
    <> traits holder
  traits (Death _ _) = mempty
  traits (GainSettlementsUnderTitle title enemy) = traits title <> traits enemy
  traits (OpinionModifier _ who _ _) = traits who
  traits (ReligionAuthority _) = mempty
  traits (RemoveOpinion _ scope _) = traits scope
  traits (ScopedModifier _ _) = mempty
  traits War {} = mempty

instance GatherTraits Modifier where
  traits (Modifier _ _) = []

instance GatherTraits Cond.Condition where
  traits (Cond.Trait t) = [t]
  traits _ = []

instance GatherTraits c => GatherTraits (Cond.Value c) where
  traits _ = []

instance GatherTraits Cond.ScopeType where
  traits _ = []

instance GatherTraits c => GatherTraits (Cond.Scope c) where
  traits (Cond.Scope _ limit cont) = traits cont <> traits limit

instance GatherTraits Event where
  traits Event { options, trigger, immediate } = traits options
                                                 <> traits trigger
                                                 <> traits immediate
            
instance GatherTraits E.Option where
  traits E.Option { optionTrigger, action, aiChance } =
    traits optionTrigger
    <> traits action
    <> traits aiChance

instance GatherTraits Decision where
  traits Decision { potential, allow, effect, aiWillDo } =
    traits potential
    <> traits allow
    <> traits effect
    <> traits aiWillDo
