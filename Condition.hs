-- | This module defines both conditions and scopes. Since there is a mutual
-- dependency, they need to go in the same module
module Condition
       (Scope(..),Condition(..),Value(..),Predicate(..),ScopeType(..)
       ,scope
       ,condition
       ,value
       ,scopedValue
       ,scopeType
       ) where

import Maker
import Scoped(Label,Atom(..))
import ScopeType(ScopeType(..),scopeType)

import Data.String(IsString(..))
import Data.List((\\))
import qualified Data.Set as S
import Data.Monoid((<>))
import qualified Data.Text as T(Text)
import Control.Applicative

-- | Wrapper for the concrete conditions
newtype Predicate = Predicate Label
                  deriving (Eq,Ord,Show)
instance Data.String.IsString Predicate where
  fromString = Predicate . fromString

-- | A @Condition@ is a boolean predicate.
data Condition = Condition Label (Value ())
                 | BooleanCondition Label Bool
                 | NumericCondition Label Double
                 | Scoped (Scope Condition)
                 | ScopedOrBoolean Label (Either Bool ScopeType)
                 | ScopedOrNumeric Label (Either Double ScopeType)
                 | VariableCheck Label (Either Label Double)
                 | Or [Condition]
                 | And [Condition]
                 | Not Condition
                 | Trait Label
                 deriving (Eq,Ord,Show)

-- | A @Value@ is used as the argument to a @Condition@ or @`Command`@
data Value a = BooleanValue Bool
           | NumValue Double
           | ScopedValue ScopeType
           | Id T.Text
           deriving (Eq,Ord,Show)
 
-- | @value@  makes a value of any non-scope, non-clause type
value = BooleanValue True <$ checkKeys ["yes","true"]
        <|> BooleanValue False <$ checkKeys ["false","no"]
        <|> NumValue <$> number
        <|> Id <$> (except (firstChild $ checkKeys ["yes","no","true","false"])
                    $ label key)

-- | @scopedValue@ makes a value of scoped type
scopedValue = ScopedValue <$> scopeType

-- | @Scope@s are used to refer to particular elements in-game, such as
-- characters or titles. Each one creates a new entry on the scope stack.
data Scope a = Scope {
  scopeType_ :: ScopeType,
  limit :: S.Set Condition,
  content :: [a]
  } deriving (Eq,Ord,Show)

-- | Make a scope
scope :: Maker a → Maker (Scope a)
scope maker = Scope <$> scopeType <*> limit <*> content
  where limit = S.fromList . concat <$> mapSubForest condition @@@ "limit"
        content = maker /@@ "limit"

predicate = label $ checkKeys unclassifiedPredicates

-- | Make a condition
condition:: Maker Condition
condition = trait
            <|> boolean
            <|> (BooleanCondition
                 <$> label (checkKeys booleanPredicates)
                 <*> fetchBool)
            <|> (NumericCondition
                 <$> label (checkKeys numericPredicates)
                 <*> firstChild number)
            <|> (ScopedOrBoolean
                 <$> label (checkKeys scopedOrBooleanPredicates)
                 <*> oneOf fetchBool (firstChild scopeType))
            <|> (ScopedOrNumeric
                 <$> label (checkKeys scopedOrNumericPredicates)
                 <*> firstChild (oneOf number scopeType))
            <|> simple
            <|> variableCheck
            <|> (Scoped <$ excludeKeys predicates <*> scope condition)
  where simple  = Condition <$> predicate <*> firstChild value
        variableCheck =
          VariableCheck <$> fetchString @@ "which" <*> (Left <$> fetchString @@ "which"
                                                       <|> Right <$> number @@ "value")
        boolean = And <$> (checkKey "AND" *> mapSubForest condition)
                  <|> Or <$> (checkKey "OR" *> mapSubForest condition)
                  <|> Not <$> (checkKey "NOT" *> firstChild condition)
        trait = Trait <$ checkKey "trait" <*> fetchString
 
predicates :: [Label]
predicates = ["random","AND","OR","NOT","calc_if_true"
             ,"age","age_diff","ai","always","attribute_diff","at_location","at_sea"
             ,"base_health","borders_major_river"
             ,"can_be_given_away","can_change_religion","can_call_crusade"
             ,"can_copy_personality_trait_from","can_have_more_leadership_traits","can_marry"
             ,"character"
             --,"check_variable"
             ,"claimed_by","combat_rating","combat_rating_diff"
             ,"completely_controls","conquest_culture","continent","controlled_by"
             ,"controls_religion","count","culture","culture_group"
             ,"defending_against_claimant","demesne_efficiency","demesne_size"
             ,"defacto_liege","de_jure_liege","de_jure_liege_or_above","de_jure_vassal_or_below"
             ,"death_reason","decadence","dynasty","difficulty","diplomacy"
             ,"diplomatic_immunity","dislike_tribal_organization","distance"
             ,"dynasty_realm_power","excommunicated_for","faction_power","family"
             ,"father_of_unborn_known","fertility","flank_has_leader","from_ruler_dynasty"
             ,"gold","graphical_culture"
             ,"had_character_flag","had_dynasty_flag","had_global_flag","had_province_flag"
             ,"had_title_flag","has_ambition","has_any_opinion_modifier","has_autocephaly"
             ,"has_building","has_called_crusade","has_character_flag"
             ,"has_character_modifier","has_claim","has_concubinage"
             ,"has_crown_law_title","has_de_jure_pretension","has_disease"
             ,"has_dlc","has_dynasty_flag","has_earmarked_regiments"
             ,"has_earmarked_regiments_not_raiding","has_embargo","has_empty_holding"
             ,"has_epidemic","has_focus","has_global_flag","has_guardian","has_heresies"
             ,"has_higher_tech_than","has_holder","has_horde_culture","has_job_action"
             ,"has_job_title","has_landed_title","has_law","has_lover","has_minor_title"
             ,"has_newly_acquired_titles","has_nickname","has_objective","has_opinion_modifier"
             ,"has_overseas_holdings","has_owner","has_plot","has_polygamy","has_province_flag"
             ,"has_province_modifier","has_raised_levies","has_regent","has_regiments"
             ,"has_siege","has_strong_claim","has_title_flag","has_trade_post","has_truce"
             ,"has_weak_claim","health","health_traits","held_title_rating"
             ,"higher_real_tier_than","higher_tier_than","holding_type","holy_order"
             ,"imprisoned_days","independent","intrigue","in_battle","in_command","in_faction"
             ,"in_revolt","in_siege","is_abroad","is_adult","is_alive","is_allied_with"
             ,"is_at_sea","is_attacker","is_betrothed","is_capital","is_chancellor"
             ,"is_child_of","is_close_relative","is_conquered","is_consort","is_contested"
             ,"is_councillor","is_crown_law_title","is_dying","is_father","is_father_real_father"
             ,"is_female","is_feudal","is_foe","is_former_lover","is_friend","is_guardian"
             ,"is_heresy_of","is_heretic","is_holy_site","is_ill","is_ironman","is_landed"
             ,"is_lowborn","is_main_spouse","is_marriage_adult","is_married"
             ,"is_married_matrilineally","is_marshal","is_merchant_republic"
             ,"is_mother","is_occupied","is_older_than","it_parent_religion"
             ,"is_patrician","is_playable","is_plot_active","is_plot_target_of"
             ,"is_pregnant","is_pretender","is_priest","is_primary_heir"
             ,"is_primary_holder_title","is_primary_title_tier","is_primary_type_title"
             ,"is_primary_war_attacker","is_primary_war_defender","is_recent_grant"
             ,"is_reformed_religion","is_reincarnated","is_republic","is_rival"
             ,"is_ruler","is_spiritual","is_spymaster","is_theocracy","is_tital_active"
             ,"is_titular","is_treasurer","is_tribal","is_tribal_type_tital"
             ,"is_valid_attraction","is_valid_viking_invasion_target","is_variable_equal"
             ,"is_vassal_or_below","is_vice_royalty","is_winter","is_within_diplo_range"
             ,"leads_faction","learning","lifestyle_traits","likes_better_than"
             ,"loot","lower_real_tier_than","lower_tier_than","martial","mercenary"
             ,"month","monthly_income","multiplayer","num_culture_realm_provs"
             ,"num_fitting_characters_for_title","num_of_baron_titles","num_of_buildings"
             ,"num_of_children","num_of_claims","num_of_consorts","num_of_count_titles"
             ,"num_of_duke_titles","num_of_dynasty_members","num_of_emperor_titles"
             ,"num_of_empty_holdings","num_of_extra_landed_titles"
             ,"num_of_faction_backers","num_of_friends","num_of_holy_sites"
             ,"num_of_king_titles","num_of_lovers","num_of_max_settlements"
             ,"num_of_plot_backers","num_of_prisoners","num_of_realm_counties"
             ,"num_of_rivals","num_of_settlements","num_of_spouses","num_of_titles"
             ,"num_of_trade_posts","num_of_trade_post_diff","num_of_traits"
             ,"num_of_unique_dynasty_vassals","num_of_vassals","num_title_realm_provs"
             ,"num_traits","opinion","opinion_diff","opinion_levy_raised_days"
             ,"overlord_of","over_max_demesne_size","over_vassal_limit","owns","pacifist"
             ,"personal_opinion","personal_opinion_diff","personality_traits","piety"
             ,"plot_is_known_by","plot_power","plot_power_contribution","port"
             ,"preparing_invasion","prestige","prisoner","province","province_id"
             ,"real_tier","realm_diplomacy","realm_intrigue","realm_martial"
             ,"realm_size","realm_stewardship","rebel","relative_power"
             ,"relative_power_to_liege","religion","religion_authority"
             ,"religion_group","reverse_has_opinion_modifier","reverse_has_truce"
             ,"reverse_opinion","reverse_personal_opinion","reverse_personal_opinion_diff"
             ,"revolt_risk","rightful_religious_head","ruled_years","same_guardian"
             ,"same_liege","same_realm","same_sex","scaled_wealth","sibling","stewardship"
             ,"trait","temporary","terrain","their_opinion","tier","title","total_claims"
             ,"treasury","troops","using_cb","vassal_of","war","war_score"
             ,"war_title","war_with","was_conceived_a_bastard","wealth"
             ,"would_be_heir_under_law","year","yearly_income"]

booleanPredicates = [
  "ai", "always", "borders_major_river", "can_be_given_away"
  , "can_change_religion", "can_call_crusade", "can_have_more_leadership_traits"
  , "controls_religion", "diplomatic_immunity"
  , "dislikes_tribal_organization", "father_of_unborn_unknown"
  , "flank_has_leader", "from_ruler_dynasty", "has_autocephaly"
  , "has_called_crusade", "has_concubinage", "has_crown_law_title"
  , "has_disease", "has_empty_holding", "has_epidemic", "has_guardian"
  , "has_heresies", "has_holder", "has_horde_culture", "has_lover"
  , "has_newly_acquired_holdings", "has_overseas_holdings", "has_owner"
  , "has_polygamy", "has_regent", "has_regiments", "has_siege", "has_trade_post"
  , "holy_order", "independent", "in_battle", "in_command", "in_revolt"
  , "in_siege", "is_abroad", "is_adult", "is_alive", "is_at_sea", "is_attacker"
  , "is_betrothed", "is_chancellor", "is_conquered"
  , "is_contested", "is_councillor", "is_crown_law_title", "is_dying"
  , "is_father_real_father", "is_female", "is_feudal", "is_former_lover"
  , "is_heretic", "is_ill", "is_ironman", "is_land", "is_landed"
  , "is_landless_type_title", "is_looting", "is_lowborn", "is_main_spouse"
  , "is_marriage_adult", "is_married_matrilineally", "is_marshal"
  , "is_merchant_republic", "is_occupied", "is_patrician", "is_playable"
  , "is_plot_active", "is_pregnant", "is_pretender", "is_priest"
  , "is_primary_holder_title", "is_primary_holder_title_tier"
  , "is_primary_type_title", "is_primary_war_attacker", "is_primary_war_defender"
  , "is_recent_grant", "is_reincarnated", "is_republic", "is_ruler"
  , "is_spiritual", "is_spymaster", "is_theocracy", "is_titular", "is_treasurer"
  , "is_tribal", "is_vice_royalty", "is_winter", "mercenary", "multiplayer"
  , "pacifist", "preparing_invasion", "prisoner", "rebel", "temporary", "war"
  , "was_conceived_a_bastard"
  ]

numericPredicates = [
  -- Double predicates
  "base_health", "demesne_efficiency", "decadence", "dynasty_realm_power"
  , "fertility", "monthly_income", "plot_power", "relative_power_to_liege"
  , "religion_authority", "revolt_risk", "scaled_wealth", "treasury", "war_score"
  , "yearly_income"
  ] <> [
  -- Integer predicates
  "age", "combat_rating", "count", "demesne_size", "diplomacy", "gold", "health"
  , "health_traits", "imprisoned_days", "intrigue", "learning"
  , "lifestyle_traits", "loot", "martial", "month"
  , "num_fitting_characters_for_title", "num_of_baron_titles", "num_of_buildings"
  , "num_of_children", "num_of_claims", "num_of_consorts", "num_of_count_titles"
  , "num_of_duke_titles", "num_of_dynasty_members", "num_of_emperor_titles"
  , "num_of_empty_holdings", "num_of_extra_landed_titles"
  , "num_of_faction_backers", "num_of_friends", "num_of_holy_sites"
  , "num_of_king_titles", "num_of_lovers", "num_of_max_settlements"
  , "num_of_plot_backers", "num_of_prisoners", "num_of_rivals"
  , "num_of_settlements", "num_of_spouses", "num_of_titles", "num_of_trade_posts"
  , "num_of_trade_post_diff", "num_of_traits", "num_of_unique_dynasty_vassals"
  , "num_of_vassals", "num_traits", "over_max_demesne_size", "over_vassal_limit"
  , "personality_traits", "piety", "prestige"
  , "real_month_of_year", "realm_diplomacy", "realm_intrigue", "realm_learning"
  , "realm_martial", "realm_size", "realm_stewardship", "ruled_years"
  , "stewardship", "wealth", "year"
  ]

scopedOrBooleanPredicates = [ "conquest_culture", "is_capital" ]
scopedOrNumericPredicates = [ "province_id" ]

unclassifiedPredicates = predicates \\ (booleanPredicates <> numericPredicates)
