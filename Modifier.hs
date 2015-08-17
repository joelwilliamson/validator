module Modifier (
  Modifier(..)
  , modifier
  ) where

import Maker(Maker,checkKeys,firstChild,label,number)
import Scoped(Label)

data Modifier = Modifier Label Double
              deriving (Eq,Ord,Show)

modifier :: Maker Modifier
modifier = Modifier
           <$> label (checkKeys modifiers)
           <*> firstChild number

modifiers =
  [ "diplomacy", "stewardship", "martial", "intrigue", "learning", "fertility"
  , "health", "combat_rating"
              
  , "demesne_size", "vassal_limit", "global_revolt_risk", "local_revolt_risk"
  , "disease_defence", "culture_flex", "religion_flex", "short_reign_length"
                                                        
  , "assassinate_chance_modifier", "arrest_chance_modifier"
  , "plot_power_modifier", "plot_discovery_chance"
                           
  , "tax_income", "global_tax_modifier", "local_tax_modifier"
  , "castle_tax_modifier", "city_tax_modifier", "temple_tax_modifier"
  , "tribal_tax_modifier", "castle_vassal_tax_modifier"
  , "city_vassal_tax_modifier", "temple_vassal_tax_modifier"
  , "tribal_vassal_tax_modifier", "monthly_character_prestige", "liege_prestige"
  , "add_prestige_modifier", "monthly_character_piety", "liege_piety"
  , "add_piety_modifier", "monthly_character_wealth"
                          
  , "ai_rationality", "ai_zeal", "ai_greed", "ai_honor", "ai_ambition"

  , "ai_feudal_modifier", "ai_republic_modifier"

  , "build_time_modifier", "local_build_time_modifier"
  , "local_build_cost_modifier"

  , "ambition_opinion", "vassal_opinion", "sex_appeal_opinion", "same_opinion"
  , "same_opinion_if_same_religion", "opposite_opinion", "liege_opinion"
  , "general_opinion", "twin_opinion", "dynasty_opinion", "male_dynasty_opinion"
  , "female_dynasty_opinion", "child_opinion", "oldest_child_opinion"
  , "youngest_child_opinion", "spouse_opinion"
                              -- Should have %religion%_opinion
  , "same_religion_opinion", "infidel_opinion", "christian_church_opinion"
  , "church_opinion", "temple_opinion", "temple_all_opinion", "town_opinion"
  , "city_opinion", "castle_opinion", "feudal_opinion", "tribal_opinion"
  , "rel_head_opinion", "free_invest_vassal_opinion"

  , "levy_size", "castle_levy_size", "city_levy_size", "temple_levy_size"
  , "tribal_levy_size", "levy_reinforce_rate"
  -- %unit%, %unit%_max_levy, %unit%_min_levy, %unit%_offensive
  -- %unit%_defensive, %unit%_morale
  , "castle_vassal_min_levy", "city_vassal_min_levy", "temple_vassal_min_levy"
  , "tribal_vassal_min_levy", "castle_vassal_max_levy", "city_vassal_max_levy"
  , "temple_vassal_max_levy", "land_moral", "land_organisation"
  , "regiment_reinforcement_speed", "garrison_size", "garrison_growth"
  , "forst_level", "global_defensive", "land", "global_supply_limit"
  , "supply_limit", "max_attrition", "siege_defence", "siege_speed"
  , "galleys_perc", "retinuesize", "retinuesize_perc"

  , "tech_growth_modifier", "tech_growth_modifier_cultural"
  , "tech_growth_modifier_economic", "tech_growth_modifier_military"
  , "cultural_techpoints", "military_techpoints", "economic_techpoints"

  , "max_tradeposts", "tradevalue"
  ]
