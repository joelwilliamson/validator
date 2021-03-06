-- | ScopeTypes are used to introduce new scopes
module ScopeType(
  ScopeType(..)
  , Tier(..)
  , scopeType
  ) where

import Scoped(Atom(Label,Number),Label())
import Maker(key)
import qualified Data.Text as T
import Data.Maybe(fromMaybe)

data Tier = Barony | County | Duchy | Kingdom | Empire
          deriving (Eq,Ord,Show)

-- | Identify the type of the element a scope references, or move around the
-- scope stack.
data ScopeType = Root | This
               | Prev | PrevPrev | PrevPrevPrev | PrevPrevPrevPrev
               | From | FromFrom | FromFromFrom | FromFromFromFrom
               | Trigger | Limit
               | EventTarget Label
               | CharacterScope Label -- This is something like "any_..." or "top_liege"
               | TitleScope Label -- any_realm_title, crusade_target
               | FixedTitle Tier Label
               | IdScope Label -- This is something like "e_..." or "%trait_name%"
               | NumScope Double -- This is a fake scope. A scope should never be a number
                 deriving (Eq,Ord,Show)


-- scopeMap associates scope names (in all lower case) with scope constructors.
-- It seems that most scopes are case insensitive.
scopeMap :: [(Label,ScopeType)]
scopeMap = [("ROOT", Root),
            ("THIS",This),
            ("PREV", Prev),
            ("PREVPREV", PrevPrev),
            ("PREVPREVPREV", PrevPrevPrev),
            ("PREVPREVPREVPREV", PrevPrevPrevPrev),
            ("FROM", From),
            ("FROMFROM", FromFrom),
            ("FROMFROMFROM", FromFromFrom),
            ("FROMFROMFROMPREV", FromFromFromFrom),
            ("TRIGGER", Trigger),
            ("LIMIT", Limit)]
readScope (Label s)
  | T.take 13 s == "event_target:" = EventTarget $ T.drop 13 s
  | T.toUpper s `elem` map fst scopeMap = fromMaybe (error "Key not in association list")
                                          $ lookup (T.toUpper s) scopeMap
  | s `elem` characterScope = CharacterScope s
  | s `elem` titleScope = TitleScope s
  | T.isPrefixOf "b_" s = FixedTitle Barony $ T.drop 2 s
  | T.isPrefixOf "c_" s = FixedTitle County $ T.drop 2 s
  | T.isPrefixOf "d_" s = FixedTitle Duchy $ T.drop 2 s
  | T.isPrefixOf "k_" s = FixedTitle Kingdom $ T.drop 2 s
  | T.isPrefixOf "e_" s = FixedTitle Empire $ T.drop 2 s
  | otherwise = IdScope s
readScope (Number n) = NumScope n

-- | Make a @ScopeType@.
scopeType = readScope <$> key

characterScope :: [Label]
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

titleScope :: [Label]
titleScope = [
  "any_claim", "any_de_jure_vassal_title", "any_direct_de_jure_vassal_title"
  , "any_demesne_title", "any_heir_title", "any_potential_tribal_county"
  , "any_pretender_title", "any_province_holding", "any_realm_title"
  , "capital_holding", "county", "crownlaw_title", "crusade_target"
  , "dejure_liege_title", "duchy", "empire", "family_palace", "invasion_target"
  , "kingdom", "new_title", "plot_target_title", "primary_title", "random_claim"
  , "random_demesne_title", "random_potential_tribal_county"
  , "random_province_holding", "random_realm_title", "supported_claimant_title"
  , "realm", "succ_law_title", "thirdparty_title_scope"
  ]
