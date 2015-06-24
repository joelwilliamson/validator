-- | This module defines both conditions and scopes. Since there is a mutual
-- dependency, they need to go in the same module
module Condition
       (Scope(..),Condition(..),Value(..),Predicate(..),ScopeType(..)
       ,Clause(..)
       ,scope
       ,condition
       ,clause
       ,value
       ,scopedValue
       ,scopeType
       ) where

import Maker
import Duration(Duration(),duration)
import Scoped(Label,Atom(..))

import qualified Data.Text as T(Text,take,drop)
import Control.Applicative

-- | Wrapper for the concrete conditions
newtype Predicate = Predicate Atom
                  deriving (Eq,Ord,Show)

-- | A @Condition@ is a boolean predicate.
data Condition = Condition Predicate (Value ())
                 | Scoped (Scope Condition)
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
           | Clause (Clause a)
           | Id T.Text
           deriving (Eq,Ord,Show)
 
-- | @value@  makes a value of any non-scope, non-clause type
value = BooleanValue True <$ checkKeys ["yes","true"]
        <|> BooleanValue False <$ checkKeys ["false","no"]
        <|> NumValue <$> number
        <|> Id <$> label key

-- | @scopedValue@ makes a value of scoped type
scopedValue = ScopedValue <$> scopeType

-- | A @Clause@ is typically used as the argument to a command. They appear in
-- the source as blocks, and don't appear to have any commonality beyond that.
data Clause a = ScopedModifier Label Duration
              | OpinionModifier Label Duration
              | TitleStatus Label Bool
              | UnknownClause [(Label,Label)]
                deriving (Eq,Ord,Show)

-- | Make a clause.
clause :: Maker (Clause a)
clause = (ScopedModifier <$ checkKeys ["add_character_modifier","add_province_modifier"]
              <*> fetchString @@ "name" <*> duration)

-- | Identify the type of the element a scope references, or move around the
-- scope stack.
data ScopeType = Root | This
               | Prev | PrevPrev | PrevPrevPrev | PrevPrevPrevPrev
               | From | FromFrom | FromFromFrom | FromFromFromFrom
               | Trigger | Limit
               | EventTarget Label
               | CharacterScope Label -- This is something like "any_..." or "top_liege"
               | IdScope Label -- This is something like "e_..." or "%trait_name%"
               | NumScope Double -- This is a fake scope. A scope should never be a number
                 deriving (Eq,Ord,Show)

readScope :: Atom → ScopeType
readScope (Label "ROOT") = Root
readScope (Label "THIS") = This
readScope (Label "PREV") = Prev
readScope (Label "PREVPREV") = PrevPrev
readScope (Label "PREVPREVPREV") = PrevPrevPrev
readScope (Label "PREVPREVPREVPREV") = PrevPrevPrevPrev
readScope (Label "FROM") = From
readScope (Label "FROMFROM") = FromFrom
readScope (Label "FROMFROMFROM") = FromFromFrom
readScope (Label "FROMFROMFROMPREV") = FromFromFromFrom
readScope (Label "trigger") = Trigger
readScope (Label "limit") = Limit
readScope (Label s)
  | T.take 13 s == "event_target:" = EventTarget $ T.drop 13 s
  | s `elem` characterScope = CharacterScope s
  | otherwise = IdScope s
readScope (Number n) = NumScope n

-- | Make a @ScopeType@.
scopeType = readScope <$> key

-- | @Scope@s are used to refer to particular elements in-game, such as
-- characters or titles. Each one creates a new entry on the scope stack.
data Scope a = Scope {
  scopeType_ :: ScopeType,
  limit :: [Condition],
  content :: [a]
  } deriving (Eq,Ord,Show)

-- | Make a scope
scope :: Maker a → Maker (Scope a)
scope maker = Scope <$> scopeType <*> limit <*> content
  where limit = condition @@@ "limit"
        content = maker /@@ "limit"

characterScope = [
  "any_allied_character","any_attacker","any_backed_character","any_child"
  ,"any_claimant","any_close_relative","any_courtier","any_current_enemy"
  ,"any_de_jure_vassal","any_defender","any_dynasty_member","any_enemy_plotter"
  ,"any_faction_backer","any_friend","any_independent_ruler"
  ,"any_known_enemy_plotter","any_known_plotter"
  ,"any_liege","any_lover","any_opinion_modifier_target","any_playable_ruler"
  ,"any_plot_backer","any_previous_holder","any_province_character"
  ,"any_province_lord","any_realm_character","any_realm_lord","any_rival"
  ,"any_sibling","any_spouse","any_consort","any_trade_post"
  ,"any_unique_dynasty_vassal","any_unknown_enemy_plotter","any_vassal"
  ,"any_ward","attacker","best_crusade_claimant","biggest_realm_size_relative"
  ,"controller","current_heir","defender","dynasty_head","employer","father"
  ,"father_even_if_dead","father_of_unborn","guardian","heir_under_seniority_law"
  ,"heir_under_primogeniture_law","highest_ranked_relative","holder_scope"
  ,"host","killer","leader","liege","liege_before_war","lover"
  ,"most_participating_attacker","most_participating_defender","mother"
  ,"mother_even_if_dead","new_character","owner","parent_religion_head"
  ,"plot_target_char",  "random_allied_character","random_backed_character"
  ,"random_child","random_courtier","random_current_enemy"
  ,"random_dynasty_member","random_enemy_plotter","random_friend"
  ,"random_independent_ruler","random_known_enemy_plotter","random_lover"
  ,"random_opinion_modifier_target","random_playable_ruler"
  ,"random_province_character","random_province_lord","random_realm_character"
  ,"random_realm_lord","random_rival","random_sibling","random_spouse"
  ,"random_consort","random_trade_post","random_unknown_enemy_plotter"
  ,"random_vassal","random_ward","real_father","reincarnation_scope","regent"
  ,"religion_head","rightful_religious_head_scope","ruler","spouse"
  ,"spouse_even_if_dead","supported_claimant","top_liege","trade_post_owner"
  ,"job_chancellor","job_marshal","job_treasurer","job_spymaster","job_spiritual"
  ]

predicate = Predicate <$> checkKeys predicates

-- | Make a condition
condition:: Maker Condition
condition = trait <|> simple <|> boolean <|> variableCheck <|> clausal <|> (Scoped <$> scope condition)
  where simple  = Condition <$> predicate <*> firstChild value
        variableCheck =
          VariableCheck <$> fetchString @@ "which" <*> (Left <$> fetchString @@ "which"
                                                       <|> Right <$> number @@ "value")
        clausal = Condition <$> predicate <*> (Clause <$> clause)
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
             ,"temporary","terrain","their_opinion","tier","title","total_claims"
             ,"trait","treasury","troops","using_cb","vassal_of","war","war_score"
             ,"war_title","war_with","was_conceived_a_bastard","wealth"
             ,"would_be_heir_under_law","year","yearly_income"]
