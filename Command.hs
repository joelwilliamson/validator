-- | Commands
module Command
       (
         Command(..),
         FlagType(..),
         Modifier(..),
         Op(..),
         command,
         stringyCommands
       )where

import Condition(Condition,Scope,ScopeType(This),Value,condition,scope,scopeType,value)
import Control.Applicative((<|>),optional)
import Data.List((\\))
import Data.Monoid((<>))
import Duration(Duration(..),duration)
import Maker
import Scoped(Label,EventId)

($>) = flip (<$)

data Op = Change | Check | Divide | Equal | Multiply | Subtract | Set deriving (Eq,Ord,Show)
op :: Maker Op
op = checkKey "set_variable" $> Set
     <|> checkKey "change_variable" $> Change
     <|> checkKey "check_variable" $> Check
     <|> checkKey "divide_variable" $> Divide
     <|> checkKey "is_variable_equal" $> Equal
     <|> checkKey "multiply_variable" $> Multiply
     <|> checkKey "subtract_variable" $> Subtract
     
-- | Specify the type of a flag being set/cleared by set_*_flag
data FlagType = Character | Province | Title | Dynasty | Global deriving (Eq,Ord,Show)
setFlag :: Maker (Label → Command)
setFlag =  SetFlag <$> (checkKey "set_character_flag" $> Character
                        <|> checkKey "set_province_flag" $> Province
                        <|> checkKey "set_title_flag" $> Title
                        <|> checkKey "set_dynasty_flag" $> Dynasty
                        <|> checkKey "set_global_flag" $> Dynasty)
           <?> "Set flag"
clrFlag :: Maker (Label → Command)
clrFlag =  ClrFlag <$> (checkKey "clr_character_flag" $> Character
                        <|> checkKey "clr_province_flag" $> Province
                        <|> checkKey "clr_title_flag" $> Title
                        <|> checkKey "clr_dynasty_flag" $> Dynasty
                        <|> checkKey "clr_global_flag" $> Dynasty)
           <?> "Clear flag"

-- | A @Modifier@ is used to change the probability of something being chosen
-- out of a random set.
data Modifier = Modifier Double [Condition]
              deriving (Eq,Ord,Show)
modifier :: Maker Modifier
modifier = Modifier <$> number ~@ "factor" <*> condition /@@ "factor"

-- | A @Command@ is an instruction to the game engine to actually change something
data Command = ActivateTitle Label Bool
             | AddTrait Label | RemoveTrait Label
             | BestFitCharacterForTitle { title :: ScopeType
                                        , perspective :: ScopeType
                                        , index :: Double
                                        , grantTitle :: ScopeType }
             | Break
             | BuildHolding Label Label ScopeType
             | ChangeTech Label Double
             | TriggerEvent EventId (Maybe Duration) (Maybe Label)
             | CreateCharacter { age :: Double
                               , name :: Label
                               , hasNickName :: Maybe Label
                               , attributes :: [Double]
                               , traits :: [Label]
                               , health :: Double
                               , fertility :: Maybe Double
                               , randomTraits :: Maybe Bool
                               , female :: Bool
                               , employer :: Maybe ScopeType
                               , religion :: ScopeType
                               , culture :: ScopeType
                               , dynasty :: Label
                               , dna :: Maybe Label
                               , flag :: Maybe Label
                               , father :: Maybe ScopeType
                               , mother :: Maybe ScopeType
                               , race :: Maybe ScopeType }
             | CreateTitle { tier :: Label
                           , landless :: Maybe Bool
                           , temporary :: Maybe Bool
                           , rebel :: Maybe Bool
                           , titleCulture :: Maybe ScopeType
                           , name :: Label
                           , holder :: ScopeType
                           , customCreated :: Maybe Bool
                           , baseTitle :: Maybe Label
                           , copyTitleLaws :: Maybe Bool }
             | Death { deathReason :: Label
                     , killer :: ScopeType }
             | GainSettlementsUnderTitle { title :: ScopeType
                                         , enemy :: ScopeType }
             | SetFlag FlagType Label | ClrFlag FlagType Label
             | If [Condition] [Command]
             | NumericCommand Label Double
             | OpinionModifier { opinionModifier :: Label
                               , who :: ScopeType
                               , me :: ScopeType
                               , dur :: Duration }
             | Random Double [Modifier] [Command]
             | RandomList [(Double,[Modifier],[Command])]
             | ReligionAuthority (Either Double Label)
             | RemoveOpinion { opinionModifier :: Label
                             , who :: ScopeType
                             , me :: ScopeType }
             | Scoped (Scope Command)
             | ScopedModifier Label Duration
             | SpawnUnit { province :: Double
                         , owner :: Maybe ScopeType
                         , leader :: Maybe ScopeType
                         , home :: Maybe ScopeType
                         , earmark :: Maybe Label
                         , troops :: [(Label,Double,Double)]
                         , matchCharacter :: Maybe ScopeType
                         , matchMult :: Maybe Double
                         , matchMax :: Maybe Double
                         , matchMin :: Maybe Double
                         , disbandOnPeace :: Maybe Bool
                         , cannotInherit :: Maybe Bool
                         , attrition :: Maybe Double
                         , maintenanceMultiplier :: Maybe Double
                         , scaledByBiggestGarrison :: Maybe Double
                         , merge :: Maybe Bool
                         }
             | VarOpLit Label Op Double
             | VarOpVar Label Op Label
             | VarOpScope Label Op (Scope ())
             | War { attacker :: ScopeType
                   , target :: ScopeType
                   , casusBelli :: Label
                   , thirdparty :: Maybe ScopeType
                   , targetTier :: Maybe Label }
             | Concrete Label (Value Command)
  deriving (Eq,Ord,Show)

-- | Make a @Command@.
command :: Maker Command
command = (ActivateTitle <$ checkKey "activate_title"
           <*> fetchString @@ "title"
           <*> fetchBool @@ "status")
          <|> (ScopedModifier <$ checkKeys ["add_character_modifier","add_province_modifier"]
               <*> fetchString @@ "name" <*> duration)

          <|> (AddTrait <$ checkKey "add_trait" <*> fetchString)
          <|> (RemoveTrait <$ checkKey "remove_trait" <*> fetchString)
          <|> (BestFitCharacterForTitle <$ checkKey "best_fit_character_for_title"
              <*> scopeType ~@ "title"
              <*> scopeType ~@ "perspective"
              <*> number ~@ "index"
              <*> scopeType ~@ "grant_title")
          <|> (Break <$ checkKey "break")
          <|> (BuildHolding <$ checkKey "build_holding"
               <*> fetchString @@ "title"
               <*> fetchString @@ "type"
               <*> scopeType ~@ "holder")
          <|> (setFlag <*> fetchString)
          <|> (clrFlag <*> fetchString)
          <|> ChangeTech <$ checkKey "change_tech" <*> fetchString @@ "technology" <*> number ~@ "value"
          <|> triggerEvent
          <|> createCharacter
          <|> createTitle
          <|> (Death <$ checkKey "death") <*> fetchString @@ "death_reason" <*> scopeType ~@ "killer"
          <|> ((GainSettlementsUnderTitle <$ checkKey "gain_settlements_under_title")
               <*> scopeType ~@ "title"
               <*> scopeType ~@ "enemy")
          <|> (If <$ checkKey "if") <*> mapSubForest condition @@ "limit" <*> command /@@ "limit"
          <|> numericCommand
          <|> (OpinionModifier <$ checkKey "opinion") <*> fetchString @@ "modifier" <*> scopeType ~@ "who" <*> pure This <*> duration
          <|> (Random <$ checkKey "random") <*> number ~@ "chance" <*> modifier @@@ "modifier" <*> command /@# ["chance","modifier"]
          <|> (RandomList <$ checkKey "random_list") <*> mapSubForest rlElem
          <|> religionAuthority
          <|> (RemoveOpinion <$ checkKey "remove_opinion") <*> fetchString @@ "modifier" <*> scopeType ~@ "who" <*> pure This
          <|> (OpinionModifier <$ checkKey "reverse_opinion") <*> fetchString @@ "modifier" <*> pure This <*> scopeType ~@ "who" <*> duration
          <|> (RemoveOpinion <$ checkKey "reverse_remove_opinion") <*> fetchString @@ "modifier" <*> pure This  <*> scopeType ~@ "who"
          <|> ((War <$ checkKey "reverse_war"
               <*> scopeType ~@ "target" -- Attacler
               <*> pure This -- Defender
               <*> fetchString @@ "casus_belli"
               -- The "thirdparty" form is used a single time in vanilla (in dynasty_events.txt).
               -- I am rather suspicious whether it is really valid.
               <*> (Just <$> scopeType ~@ "thirdparty_title" <|> scopeType ~? "thirdparty")
               <*> fetchString @? "tier") <?> "Reverse War")
          <|> VarOpLit <$> firstChild (checkKey "which" *> fetchString) <*> op <*> number ~@ "value"
          <|> VarOpVar <$> firstChild (checkKey "which" *> fetchString) <*> op <*> secondChild (checkKey "which" *> fetchString) -- This doesn't distinguish between scopes and variables in the same scope
          <|> (SpawnUnit <$ checkKey "spawn_unit"
               <*> number ~@ "province"
               <*> scopeType ~? "owner"
               <*> scopeType ~? "leader"
               <*> scopeType ~? "home"
               <*> label key ~? "earmark"
               <*> mapSubForest troopSpec @@ "troops"
               <*> scopeType ~? "match_character"
               <*> number ~? "match_mult"
               <*> number ~? "match_max"
               <*> number ~? "match_min"
               <*> fetchBool @? "disband_on_peace"
               <*> fetchBool @? "cannot_inherit"
               <*> number ~? "attrition"
               <*> number ~? "maintenance_multiplier"
               <*> number ~? "scaled_by_biggest_garrison"
               <*> fetchBool @? "merge"
              )
          <|> ((War <$ checkKey "war"
               <*> pure This -- Attacker
               <*> scopeType ~@ "target"
               <*> fetchString @@ "casus_belli"
               <*> scopeType ~? "thirdparty_title"
               <*> fetchString @? "tier") <?> "War")
          <|> Concrete <$> label (checkKeys concreteCommands) <*> firstChild value
          <|> (Scoped <$ excludeKeys commands) <*> scope command

  where rlElem = (,,) <$> number <*> modifier @@@ "modifier" <*> command /@@ "modifier"
        troopSpec = (,,) <$> label key <*> firstChild number <*> firstChild (firstChild number)

triggerEvent = TriggerEvent <$ checkKeys ["character_event","letter_event","narrative_event","province_event","repeat_event"]
                 <*> fetchId @@ "id"
                 <*> optional duration
                 <*> fetchString @? "tooltip"

createCharacter = CreateCharacter <$ checkKeys ["create_character"
                                               ,"create_random_diplomat"
                                               ,"create_random_intriguer"
                                               ,"create_random_priest"
                                               ,"create_random_soldier"
                                               ,"create_random_steward"]
                  <*> number ~@ "age"
                  <*> fetchString @@ "name"
                  <*> fetchString @? "has_nickname"
                  <*> mapSubForest (firstChild number) @@ "attributes"
                  <*> fetchString @@@ "trait"
                  <*> number ~@ "health"
                  <*> number ~? "fertility"
                  <*> fetchBool @? "random_traits"
                  <*> fetchBool @@ "female"
                  <*> scopeType ~? "employer"
                  <*> scopeType ~@ "religion"
                  <*> scopeType ~@ "culture"
                  <*> fetchString @@ "dynasty"
                  <*> fetchString @? "dna"
                  <*> fetchString @? "flag"
                  <*> scopeType ~? "father"
                  <*> scopeType ~? "mother"
                  <*> scopeType ~? "race"

createTitle = CreateTitle <$ checkKey "create_title"
              <*> fetchString @@ "tier"
              <*> fetchBool @? "landless"
              <*> fetchBool @? "temporary"
              <*> fetchBool @? "rebel"
              <*> scopeType ~? "culture"
              <*> fetchString @@ "name"
              <*> scopeType ~@ "holder"
              <*> fetchBool @? "custom_created"
              <*> fetchString @? "base_title"
              <*> fetchBool ~? "copy_title_law"

numericCommand = NumericCommand <$ checkKeys numericCommands
                 <*> label key
                 <*> firstChild number

religionAuthority = (ReligionAuthority <$ checkKey "religion_authority")
                    <*> (Left <$> firstChild number
                         <|> Right <$> fetchString @@ "modifier")

commands =
  ["abandon_heresy","abdicate","abdicate_to","abdicate_to_most_liked_by",
   "activate_disease","activate_plot","activate_title","add_ambition",
   "add_betrothal","add_building","add_character_modifier","add_claim",
   "add_consort","add_friend","add_law","add_law_w_cooldown","add_lover",
   "add_number_to_name","add_objective","add_piety_modifier","add_pressed_claim",
   "add_prestige_modifier","add_province_modifier","add_rival","add_spouse",
   "add_spouse_matrilineal","add_trait","add_weak_claim","add_weak_pressed_claim",
   "adjective","ambition_succeeds","approve_law","back_plot","banish","banish_religion",
   "become_heretic","best_fit_character_for_title","break_betrothal","build_holding",
   "cancel_ambition","cancel_job_action","cancel_objective","cancel_plot","capital",
   "change_diplomacy","change_intrigue","change_learning","change_martial",
   "change_phase_to","change_random_civ_tech","change_random_eco_tech","change_random_mil_tech",
   "change_stewardship","change_tech","change_variable","character_event",
   "clear_event_target","clear_global_event_target","clear_global_event_targets",
   "clear_revolt","clear_wealth","clr_character_flag","clr_dynasty_flag","clr_global_flag",
   "clr_province_flag","clr_title_flag","conquest_culture","convert_to_castle",
   "convert_to_city","convert_to_temple","convert_to_tribal","copy_random_personality_trait",
   "copy_title_history","copy_title_laws","create_character","create_family_palace",
   "create_random_diplomat","create_random_intriguer","create_random_priest",
   "create_random_soldier","create_random_steward","create_title","chronicle","culture",
   "culture_techpoints","cure_illness","de_jure_liege","death",
   "decadence","destroy_landed_title","destroy_random_building","destroy_tradepost",
   "diplomatic_immunity","disband_event_forces","divide_variable","dynasty",
   "economy_techpoints","enable_prepared_invasion","end_war","embargo",
   "excommunicate","faction","fertility","gain_all_occupied_titles",
   "gain_settlements_under_title","gain_title","gain_title_plus_barony_if_unlanded",
   "gender_succ","give_job_title","give_minor_title","give_nickname","gold",
   "grant_kingdom_w_adjudication","grant_title","grant_title_no_opinion","health",
   "hold_election","impregnate","impregnate_cuckoo","imprison",
   "inherit","join_attacker_wars","join_defender_wars","leave_plot",
   "log","letter_event","make_primary_spouse","make_primary_title",
   "military_techpoints","move_character","multiply_variable","narrative_event",
   "occupy_minors_of_occupied_settlements","opinion","participation_scaled_decadence",
   "participation_scaled_piety",
   "participation_scaled_prestige","piety","plot_succeeds","press_claim",
   "prestige","province_capital","province_event","random",
   "random_list","rebel_defection","recalc_succession","reduce_disease",
   "refill_holding_levy","religion_authority","remove_building","remove_character_modifier",
   "remove_claim","remove_consort","remove_friend","remove_holding_modifier",
   "remove_lover","remove_nickname","remove_opinion","remove_province_modifier",
   "remove_rival","remove_settlement","remove_spouse","remove_title",
   "remove_trait","repeat_event","reset_coa","reveal_plot",
   "reveal_plot_w_message","reverse_banish","reverse_culture","reverse_imprison",
   "reverse_opinion","reverse_religion","reverse_remove_opinion","reverse_war",
   "revoke_law","save_event_target_as","save_global_event_target_as","scaled_wealth",
   "seize_trade_post","send_assassin","set_allow_free_duchy_revokation","set_allow_free_infidel_revokation",
   "set_allow_free_revokation","set_allow_title_revokation",
   "set_allow_free_vice_royalty_revokation","set_allow_vice_royalties",
   "set_appoint_generals","set_appoint_regents","set_character_flag","set_coa",
   "set_defacto_liege","set_defacto_vassal","set_dynasty_flag","set_father",
   "set_global_flag","set_graphical_culture","set_guardian","set_investiture",
   "set_mother","set_name","set_opinion_levy_raised_days","set_parent_religion",
   "set_protected_inheritance","set_province_flag","set_real_father","set_reincarnation",
   "set_the_kings_full_peace","set_the_kings_peace","set_title_flag","set_tribal_vassal_levy_control",
   "set_tribal_vassal_tax_income","set_variable","spawn_fleet","spawn_unit",
   "steal_random_tech","subjugate_or_take_under_title","subtract_variable","succession",
   "succession_w_cooldown","transfer_scaled_wealth","treasury"
  ,"unsafe_destroy_landed_title","unsafe_religion","usurp_title"
  ,"usurp_title_only","usurp_title_plus_barony_if_unlanded",
   "usurp_title_plus_barony_if_unlanded","usurp_title_plus_barony_if_unlanded",
   "vassal_opinion","vassalize_or_take_under_title",
   "vassalize_or_take_under_title_destroy_duchies","war","wealth"]

numericCommands =
  [ "add_piety_modifier","add_prestige_modifier","participation_scaled_decadence"
  , "participation_scaled_piety", "participation_scaled_prestige"
  , "reduce_disease", "scaled_wealth"] <>
  [ "change_diplomacy", "change_intrigue", "change_learning", "change_martial"
  , "change_random_civ_tech", "change_random_eco_tech", "change_random_mil_tech"
  , "change_stewardship", "culture_techpoints", "decadence"
  , "economy_techpoints", "fertility", "health", "military_techpoints"
  , "piety", "prestige", "treasury" ]

-- | A list of all commands that should be accepted as arguments to @`Concrete`@
concreteCommands = commands \\ ["activate_title",
                                "add_character_modifier", "add_province_modifier",
                                "add_trait",
                                "remove_trait",
                                "best_fit_character_for_title",
                                "break",
                                "build_holding",
                                "change_tech",
                                "character_event",
                                "create_character",
                                "create_random_diplomat",
                                "create_random_intriguer",
                                "create_random_priest",
                                "create_random_soldier",
                                "create_random_steward",
                                "create_title",
                                "clr_character_flag","set_character_flag",
                                "clr_dynasty_flag","set_dynasty_flag",
                                "clr_global_flag","set_global_flag",
                                "clr_province_flag","set_province_flag",
                                "clr_title_flag","set_title_flag",
                                "death",
                                "gain_settlements_under_title",
                                "if",
                                "letter_event",
                                "opinion",
                                "province_event",
                                "random","random_list",
                                "religion_authority",
                                "remove_opinion",
                                "repeat_event",
                                "reverse_opinion", "reverse_remove_opinion",
                                "spawn_unit",
                                "change_variable","check_variable",
                                "divide_variable","is_equal_variable",
                                "multiply_variable","subtract_variable",
                                "set_variable",
                                "war","reverse_war"]
                   <> numericCommands

-- | A list of all commands whose argument is a string-like key.
--
-- E.g. @activate_disease=smallpox@ or @gain_title=e_persia@
stringyCommands :: [Label]
stringyCommands =   [
   "activate_disease","add_ambition","add_law","add_law_w_cooldown","adjective",
   "add_building","add_objective","add_trait","approve_law","banish_religion","cancel_job_action",
   "change_phase_to","clear_event_target","clear_global_event_target",
   "clear_global_event_targets",
   "clr_character_flag","clr_dynasty_flag","clr_global_flag",
   "clr_province_flag","clr_title_flag",
   "copy_title_history","copy_title_laws","culture","de_jure_liege",
   "disband_event_forces","enable_prepared_invasion","end_war",
   "excommunicate","faction","fertility","gain_all_occupied_titles",
   "gain_title","gain_title_plus_barony_if_unlanded",
   "gender_succ","give_job_title","give_minor_title","give_nickname",
   "grant_title_no_opinion","log","remove_building","remove_character_modifier",
   "remove_holding_modifier","remove_nickname","remove_province_modifier",
   "remove_settlement","remove_spouse","remove_title","remove_trait",
   "revoke_law","save_event_target_as","save_global_event_target_as",
   "set_graphical_culture","set_investiture",
   "set_name","succession","succession_w_cooldown","unsafe_religion"]
