-- | This module provides the data type and parser for a trait file

module Trait (
  Trait(..)
  , defaultTrait
  , trait
  ) where

import Maker
import Modifier
import Scoped(Label)

data Trait = Trait {
  trait_name :: Label
  , agnatic :: Bool
  , birth :: Double -- ^ Chance of being assigned on birth. Default 0
  , cached :: Bool
  , cannot_inherit :: Bool
  , cannot_marry :: Bool
  , caste_tier :: Maybe Int -- ^ The trait is a caste trait, and this
                  -- defines the order of the castes.
  , customizer :: Bool -- ^ Blocks the trait from being available in the Designer
  , education :: Bool
  , immortal :: Bool
  , in_hiding :: Bool
  , inbred :: Bool
  , incapacitating :: Bool
  , inherit_chance :: Double
  , is_epidemic :: Bool
  , is_health :: Bool
  , is_illness :: Bool
  , leader :: Bool
  , leadership_traits :: Maybe Int
  , lifestyle :: Bool
  , opposites :: [Label]
  , personality :: Bool
  , prevent_decadence :: Bool
  , priest :: Bool
  , pilgrimage :: Bool
  , random :: Bool
  , rebel_inherited :: Bool -- ^ Unknown purpose
  , religious :: Bool
  , religious_branch :: Maybe Label
  , ruler_designer_cost :: Maybe Int -- ^ The postive cost in the Ruler Designer
  , tolerates :: [Label] -- ^ A list of the religion groups tolerated by this character
  , modifiers :: [Modifier]
  } deriving (Eq, Ord, Show)

trait :: Maker Trait
trait = Trait
        <$> label key
        <*> boolProp "agnatic"
        <*> ((number ~@ "birth") `defaultingTo` 0)
        <*> boolProp "cached"
        <*> boolProp "cannot_inherit"
        <*> boolProp "cannot_marry"
        <*> intProp "caste_tier"
        <*> boolProp "customizer"
        <*> boolProp "education"
        <*> boolProp "immortal"
        <*> boolProp "in_hiding"
        <*> boolProp "inbred"
        <*> boolProp "incapacitating"
        <*> (number ~@ "inherit_chance") `defaultingTo` 0
        <*> boolProp "is_epidemic"
        <*> boolProp "is_health"
        <*> boolProp "is_illness"
        <*> boolProp "leader"
        <*> intProp "leadership_traits"
        <*> boolProp "lifestyle"
        <*> (opposites @@ "opposites") `defaultingTo` []
        <*> boolProp "personality"
        <*> boolProp "prevent_decadence"
        <*> boolProp "priest"
        <*> boolProp "pilgrimage"
        <*> boolProp "random"
        <*> boolProp "rebel_inherited"
        <*> boolProp "religious"
        <*> fetchString @? "religious_branch"
        <*> intProp "ruler_designer_cost"
        <*> tolerations
        <*> tryMap modifier
  where boolProp key = ((fetchBool @@ key) `defaultingTo` False) <?> key
        intProp :: Label → Maker (Maybe Int)
        intProp key = fmap round <$> number ~? key <?> key
        opposites = error "opposite traits are not implemented"
        tolerations = return []

defaultTrait :: Trait
defaultTrait =
  Trait { trait_name = undefined, agnatic = False, birth = 0, cached = False
        , cannot_inherit = False, cannot_marry = False, caste_tier = Nothing
        , customizer = False, education = False, immortal = False
        , in_hiding = False, inbred = False, incapacitating = False
        , inherit_chance = 0, is_epidemic = False, is_health = False
        , is_illness = False, leader = False, leadership_traits = Nothing
        , lifestyle = False, opposites = [], personality = False
        , prevent_decadence = False, priest = False, pilgrimage = False
        , random = False, rebel_inherited = False, religious = False
        , religious_branch = Nothing, ruler_designer_cost = Nothing
        , tolerates = [], modifiers = [] }
