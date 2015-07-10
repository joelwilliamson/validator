module Tests.Command where

import AttoScoped(statefulParseOnly,value)
import Condition(Value(..),ScopeType(..),Condition(..),Predicate(..))
import Command
import Duration(Duration(..),duration)
import Maker(runMaker)
import Scoped(Error())

import Data.Attoparsec.ByteString
import Text.Parsec.Pos(initialPos)
import Data.Text(Text,unlines)
import Data.Either(isLeft)

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (unlines)

commandUnitTests = testGroup "Command Unit Tests"
                   [ commandSuccessTests, commandFailTests]

commandSuccessTests = testGroup "Command Success Tests"
                   [ successTest "A simple concrete command"
                     "set_coa = e_byzantium" $ Concrete "set_coa" (Id "e_byzantium")
                   , successTest "Boolean command"
                     "abandon_heresy = yes" $ BooleanCommand "abandon_heresy" True
                   , successTest "Numeric command"
                     "change_diplomacy = 3" $ NumericCommand "change_diplomacy" 3
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
                       , matchCharacter = Nothing, matchMult = Nothing
                       , matchMax = Nothing, matchMin = Nothing
                       , disbandOnPeace = Nothing, cannotInherit = Nothing
                       , maintenanceMultiplier = Nothing
                       , scaledByBiggestGarrison = Nothing, merge = Nothing
                       }
                   , successTest "Spawn a unit - 2"
                     "spawn_unit = { province = 101 home = ROOT owner = ROOT leader = ROOT earmark = test_army troops = { archers = { 300 300 } knights = { 200 200 } } match_character = THIS match_mult = 2 match_max = 10000 match_min = 100 disband_on_peace = no cannot_inherit = no attrition = 0.1 maintenance_multiplier = 0.1 scaled_by_biggest_garrison = 2.0 merge = yes }"
                     SpawnUnit {
                       province = 101, home = Just Root, owner = Just Root
                       , leader = Just Root, earmark = Just "test_army"
                       , troops = [("archers",300,300),("knights",200,200)]
                       , matchCharacter = Just This, matchMult = Just 2
                       , matchMax = Just 10000, matchMin = Just 100
                       , disbandOnPeace = Just False, cannotInherit = Just False
                       , attrition = Just 0.1, maintenanceMultiplier = Just 0.1
                       , scaledByBiggestGarrison = Just 2, merge = Just True }
                   , successTest "Break"
                     "break = yes"
                     Break
                   , successTest "Random Break"
                     "random = { chance = 20 modifier = { factor = 0.5 trait = sloth } modifier = { factor = 2 trait = diligent } break = yes }"
                     $ Random 20 [Modifier 0.5 [Trait "sloth"], Modifier 2 [Trait "diligent"]] [Break]
                   , successTest "If"
                     "if = { limit = { trait = humble prestige = 5 } change_diplomacy = 2 add_trait = monk}"
                     $ If [Trait "humble", Condition (Predicate "prestige") $ NumValue 5] [NumericCommand "change_diplomacy" 2, AddTrait "monk"]
                   , successTest "Random List"
                     "random_list = { 10 = { wealth = 10 } 20 = { unsafe_religion = catholic modifier = { factor = 2 trait = cynical }} 70 = { prestige = 30 } }"
                     $ RandomList [(10,[],[Concrete "wealth" $ NumValue 10]),
                                   (20,[Modifier 2 [Trait "cynical"]],[StringCommand "unsafe_religion" "catholic"]),
                                   (70,[],[NumericCommand "prestige" 30])]
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
                         age = Just 40
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Just 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Just Root
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
                         age = Just 40
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Just 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Just Root
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
                   , successTest "Add Character Modifier"
                     "add_character_modifier = { name = test_mod duration = 180 }"
                     $ ScopedModifier "test_mod" $ Days 180
                   , successTest "Add province modifier"
                     "add_province_modifier = { name = test_mod years = 3 }"
                     $ ScopedModifier "test_mod" $ Years 3
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
                       $ OpinionModifier { opinionModifier = "test_modifier"
                                         , who = PrevPrev
                                         , me = This
                                         , dur = Years 5 }
                     , successTest "Province event"
                       "province_event = { id = 2254 }"
                       $ TriggerEvent ("",2254) Nothing Nothing
                     , successTest "Religion authority - Numeric"
                       "religion_authority = 10"
                       $ ReligionAuthority $ Left 10
                     , successTest "Religion authority - Modifier"
                       "religion_authority = { modifier = test_mod }"
                       $ ReligionAuthority $ Right "test_mod"
                     , successTest "Remove opinion"
                       "remove_opinion = { who = FROM modifier = opinion_friend }"
                       $ RemoveOpinion  { opinionModifier = "opinion_friend"
                                        , who = From, me = This }
                     , successTest "Repeat event"
                       "repeat_event = { id = WoL.5502 days = 30 }"
                       $ TriggerEvent ("WoL",5502) (Just $ Days 30) Nothing
                     , successTest "Reverse opinion"
                       "reverse_opinion = { modifier = test_modifier years = 5 who = PREVPREV }"
                       $ OpinionModifier { opinionModifier = "test_modifier"
                                         , who = This
                                         , me = PrevPrev
                                         , dur = Years 5 }
                     , successTest "Reverse remove opinion"
                       "reverse_remove_opinion = { modifier = opinion_friend who = FROM }"
                       $ RemoveOpinion { opinionModifier = "opinion_friend"
                                       , who = This, me = From }
                     , successTest "Declare war"
                       "war = { target = PREVPREV casus_belli = duchy_adventure thirdparty_title = PREV tier = DUKE }"
                       $ War { attacker = This, target = PrevPrev
                             , casusBelli = "duchy_adventure"
                             , thirdparty = Just Prev, targetTier = Just "DUKE" }
                     , successTest "Declare reverse war"
                       "reverse_war = { target = FROM casus_belli = other_claim thirdparty = ROOT }"
                       $ War { attacker = From, target = This
                             , casusBelli = "other_claim"
                             , thirdparty = Just Root, targetTier = Nothing }
                     , successTest "Add piety modifier"
                       "add_piety_modifier = 0.3"
                       $ NumericCommand "add_piety_modifier" 0.3
                     , successTest "Remove building"
                       "remove_building = castle_town"
                       $ StringCommand "remove_building" "castle_town"
                     , successTest "Character with unspecified age"
                     (unlines ["create_random_soldier = {"
                              , "random_traits = no"
                              , "name = \"Hassan\""
                              , "dynasty = random"
                              , "religion = ROOT"
                              , "culture = persian"
                              , "female = no"
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
                         age = Nothing
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Just 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Just Root
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                      , successTest "Character with unspecified religion"
                     (unlines ["create_random_soldier = {"
                              , "random_traits = no"
                              , "name = \"Hassan\""
                              , "dynasty = random"
                              , "culture = persian"
                              , "female = no"
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
                         age = Nothing
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Just 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Nothing
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                       , successTest "Character with unspecified gender"
                     (unlines ["create_random_soldier = {"
                              , "random_traits = no"
                              , "name = \"Hassan\""
                              , "dynasty = random"
                              , "culture = persian"
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
                         age = Nothing
                         , name = "Hassan"
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Just 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Nothing
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                        , successTest "Character with unspecified name"
                     (unlines ["create_random_soldier = {"
                              , "random_traits = no"
                              , "dynasty = random"
                              , "culture = persian"
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
                         age = Nothing
                         , name = ""
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Just 6
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Nothing
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                         , successTest "Character with unspecified health"
                     (unlines ["create_random_soldier = {"
                              , "random_traits = no"
                              , "dynasty = random"
                              , "culture = persian"
                              , "has_nickname = the_testy"
                              , "attributes = {"
                              , "martial = 6"
                              , "diplomacy = 8"
                              , "stewardship = 9"
                              , "intrigue = 12"
                              , "learning = 12"
                              , "}"
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
                         age = Nothing
                         , name = ""
                         , hasNickName = Just "the_testy"
                         , attributes = [6,8,9,12,12]
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Nothing
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Nothing
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                         , successTest "Character with unspecified attributes"
                           (unlines ["create_random_soldier = {"
                              , "random_traits = no"
                              , "dynasty = random"
                              , "culture = persian"
                              , "has_nickname = the_testy"
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
                         age = Nothing
                         , name = ""
                         , hasNickName = Just "the_testy"
                         , attributes = []
                         , traits = ["elusive_shadow","patient","zealous",
                                    "scholar","chaste","temperate"]
                         , health = Nothing
                         , fertility = Just 0.8
                         , randomTraits = Just False
                         , female = False
                         , employer = Just This
                         , religion = Nothing
                         , culture = IdScope "persian"
                         , dynasty = "random"
                         , dna = Just "DNA"
                         , flag = Just "test_flag"
                         , mother = Just From
                         , father = Just FromFrom
                         , race = Just $ IdScope "testish"
                         })
                       ]

commandFailTests =
  testGroup "Failing tests"
  [ failTest "Missing brace" "opinion = { who = ROOT modifier = test years = 4"
  , failTest "Ill-formed opinion" "opinion = { who = ROOT }"
  , failTest "Event missing id" "letter_event = { days = 5 }"
  , failTest "War missing target" "war = { casus_belli = duchy }"
  , failTest "War missing CB" "war = { target = ROOT }"
  , failTest "Empty clause" "war = { }"
  , failTest "Numeric commands can't have string args" "change_diplomacy = more"
  , failTest "Numeric commands can't have clause args" "reduce_disease = { value = 3 }"
  , failTest "String commands don't take numbers" "unsafe_religion = 5"
  ]

makeCommand :: Text -> Either Error Command
makeCommand s = case statefulParseOnly value (initialPos "test_data") s of
  Left _ -> Left ("",Nothing)
  Right s -> runMaker command s

successTest :: TestName -> Text -> Command -> TestTree
successTest name command result = testCase name $ makeCommand command @?= Right result

failTest name command = testCase name $ isLeft (makeCommand command) @?= True
