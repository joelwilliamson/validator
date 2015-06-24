module Tests.Command where

import AttoScoped(statefulParseOnly,value)
import Condition(Clause(..),Value(..),ScopeType(..),Condition(..),Predicate(..))
import Command
import Duration(Duration(..),duration)
import Maker(runMaker)
import Scoped(Error())

import Data.Attoparsec.ByteString
import Text.Parsec.Pos(initialPos)
import Data.Text(Text,unlines)

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (unlines)

commandUnitTests = testGroup "Command Unit Tests"
                   [ successTest "A simple concrete command"
                     "abandon_heresy = yes" $ Concrete "abandon_heresy" (BooleanValue True)
                   , successTest "Numeric command"
                     "change_diplomacy = 3" $ Concrete "change_diplomacy" (NumValue 3)
                   , successTest "Adding a trait"
                     "add_trait = diligent" $ AddTrait "diligent"
                   , successTest "Removing a trait"
                     "remove_trait = wroth" $ RemoveTrait "wroth"
                   , successTest "Set Character Flag"
                     "set_character_flag = char_flag" $ SetFlag Character "char_flag"
                   , successTest "Clear Province Flag"
                     "clr_province_flag = prov_flag" $ ClrFlag Province "prov_flag"
                   , successTest "Set a variable"
                     "set_variable = { which = a which = b }" $ VarOpVar "a" Set "b"
                   , successTest "Add two variables"
                     "change_variable = { which = a which = b }" $ VarOpVar "a" Change "b"
                   , successTest "Check a variable against constant"
                     "check_variable = { which = a_variable value = 10 }"
                     $ VarOpLit "a_variable" Check 10
                   , successTest "Divide a variable by a constant"
                     "divide_variable = { which = a  value = 4 } "
                     $ VarOpLit "a" Divide 4
                   , successTest "Check variable strict equality"
                     "is_variable_equal = { which = a which = b }"
                     $ VarOpVar "a" Equal "b"
                   , successTest "Subtract constant from variable"
                     "subtract_variable = { which = a value = 4 }" $ VarOpLit "a" Subtract 4
                   , successTest "Multiply two variables"
                     "multiply_variable = { which = \"quoted_variable\" which = b }"
                     $ VarOpVar "quoted_variable" Multiply "b"
                   , successTest "Spawn a unit"
                     "spawn_unit = { province = 342 owner = THIS leader = FROM home = PREV attrition = 1 troops = { archers = { 100 100 } pikemen = { 200 300 }} earmark = test_troops }"
                     $ SpawnUnit {
                       province = 342
                       , owner = Just This
                       , leader = Just From
                       , home = Just Prev
                       , attrition = Just 1
                       , troops = [("archers",100,100),
                                   ("pikemen",200,300)]
                       , earmark = Just "test_troops"
                       }
                   , successTest "Break"
                     "break = yes"
                     Break
                   , successTest "Random Break"
                     "random = { chance = 20 modifier = { factor = 0.5 trait = sloth } modifier = { factor = 2 trait = diligent } break = yes }"
                     $ Random 20 [Modifier 0.5 [Trait "sloth"], Modifier 2 [Trait "diligent"]] [Break]
                   , successTest "If"
                     "if = { limit = { trait = humble prestige = 5 } change_diplomacy = 2 add_trait = monk}"
                     $ If [Trait "humble", Condition (Predicate "prestige") $ NumValue 5] [Concrete "change_diplomacy" $ NumValue 2, AddTrait "monk"]
                   , successTest "Random List"
                     "random_list = { 10 = { wealth = 10 } 20 = { unsafe_religion = catholic modifier = { factor = 2 trait = cynical }} 70 = { prestige = 30 } }"
                     $ RandomList [(10,[],[Concrete "wealth" $ NumValue 10]),
                                   (20,[Modifier 2 [Trait "cynical"]],[Concrete "unsafe_religion" $ Id "catholic"]),
                                   (70,[],[Concrete "prestige" $ NumValue 30])]
                   , successTest "Create Character"
                     (unlines ["create_character = {"
                              , "random_traits = no"
                              , "name = \"Hassan\""
                              , "dynasty = random"
                              , "religion = ROOT"
                              , "culture = persian"
                              , "female = no"
                              , "age = 40"
                              , "has_nickname = the_testy"
                              , "attributes = {"
                              , "martial = 6"
                              , "diplomacy = 8"
                              , "stewardship = 9"
                              , "intrigue = 12"
                              , "learning = 12"
                              , "}"
                              , "health = 6"
                              , "fertility = 0.8"
                              , "mother = FROM"
                              , "father = FROMFROM"
                              , "race = testish"
                              , "dna = DNA"
                              , "flag = \"test_flag\""
                              , "employer = THIS"
                              , "trait = elusive_shadow"
                              , "trait = patient"
                              , "trait = zealous"
                              , "trait = scholar"
                              , "trait = chaste"
                              , "trait = temperate }"])
                     (CreateCharacter {
                         age = 40
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Root
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                   , successTest "Create Character"
                     (unlines ["create_random_intriguer = {"
                              , "random_traits = no"
                              , "name = \"Hassan\""
                              , "dynasty = random"
                              , "religion = ROOT"
                              , "culture = persian"
                              , "female = no"
                              , "age = 40"
                              , "has_nickname = the_testy"
                              , "attributes = {"
                              , "martial = 6"
                              , "diplomacy = 8"
                              , "stewardship = 9"
                              , "intrigue = 12"
                              , "learning = 12"
                              , "}"
                              , "health = 6"
                              , "fertility = 0.8"
                              , "mother = FROM"
                              , "father = FROMFROM"
                              , "race = testish"
                              , "dna = DNA"
                              , "flag = \"test_flag\""
                              , "employer = THIS"
                              , "trait = elusive_shadow"
                              , "trait = patient"
                              , "trait = zealous"
                              , "trait = scholar"
                              , "trait = chaste"
                              , "trait = temperate }"])
                     (CreateCharacter {
                         age = 40
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Root
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                   , successTest "activate title"
                     "activate_title = { title = e_persia status = yes }"
                     $ ActivateTitle "e_persia" True
                   , successTest "deactivate title"
                     "activate_title = { title = b_rome status = no }"
                     $ ActivateTitle "b_rome" False
                   , successTest "character event"
                     "character_event = { id = test.12 days = 5 tooltip = \"An event\" }"
                     $ TriggerEvent ("test",12) (Just $ Days 5) (Just "An event")
                   , successTest "Build holding"
                     "build_holding = { title = b_masyaf type = castle holder = ROOT }"
                     $ BuildHolding "b_masyaf" "castle" Root
                   , successTest "Change legalism"
                     "change_tech = { technology = TECH_LEGALISM value = 1 }"
                     $ ChangeTech "TECH_LEGALISM" 1
                     , successTest "Best Fit Character"
                       (unlines ["best_fit_character_for_title = {",
                                 "title = PREV",
                                 "perspective = ROOT",
                                 "index = 1",
                                 "grant_title = PREV",
                                 "}"])
                       $ BestFitCharacterForTitle {
                         title = Prev
                         , perspective = Root
                         , index = 1
                         , grantTitle = Prev }
                     , successTest "Create title" "create_title = { tier = DUKE name = \"SHEPHERDS_CRUSADE\" holder = THIS }"
                       $ CreateTitle { tier = "DUKE"
                                     , landless = Nothing
                                     , temporary = Nothing
                                     , rebel = Nothing
                                     , titleCulture = Nothing
                                     , name = "SHEPHERDS_CRUSADE"
                                     , holder = This
                                     , customCreated = Nothing
                                     , baseTitle = Nothing
                                     , copyTitleLaws = Nothing }
                     , successTest "Death" "death = { death_reason = death_disease killer = FROMFROMFROM }"
                       $ Death { deathReason = "death_disease"
                               , killer = FromFromFrom }
                     , successTest "Gain Settlements"
                       "gain_settlements_under_title = { title = PREV enemy = ROOT }"
                       GainSettlementsUnderTitle { title = Prev, enemy = Root }
                     , successTest "Letter Event with tooltip"
                       "letter_event = { id = 40 months = 3 tooltip = EVTTESTTOOLTIP }"
                       $ TriggerEvent ("",40) (Just $ Months 3) $ Just "EVTTESTTOOLTIP"
                     , successTest "Minimal letter event"
                       "letter_event = { id = test.3 }"
                       $ TriggerEvent ("test",3) Nothing Nothing
                     , successTest "Opinion modifier"
                       "opinion = { modifier = test_modifier years = 5 who = PREVPREV }"
                       $ OpinionModifier "test_modifier" PrevPrev (Years 5)
                   ]

makeCommand :: Text -> Either Error Command
makeCommand s = case statefulParseOnly value (initialPos "test_data") s of
  Left _ -> Left ("",Nothing)
  Right s -> runMaker command s

successTest :: TestName -> Text -> Command -> TestTree
successTest name command result = testCase name $ makeCommand command @?= Right result
