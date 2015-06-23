module Tests.Command where

import AttoScoped(statefulParseOnly,value)
import Condition(Clause(..),Value(..),ScopeType(..),Condition(..),Predicate(..))
import Command
import Maker(runMaker)
import Scoped(Error())

import Data.Attoparsec.ByteString
import Text.Parsec.Pos(initialPos)
import Data.Text(Text)

import Test.Tasty
import Test.Tasty.HUnit

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
                     "spawn_unit = { province = 342 owner = THIS leader = FROM home = PREV attrition = 1 troops = { archers = { 100 100 } }}"
                     $ SpawnUnit {
                       province = 342
                       , owner = Just This
                       , leader = Just From
                       , home = Just Prev
                       , earmark = Nothing
                       , attrition = Just 1
                       , troops = Troops "archers" 100 100 }
                   , successTest "Break"
                     "break = yes"
                     Break
                   , successTest "Random Break"
                     "random = { chance = 20 modifier = { factor = 0.5 trait = sloth } modifier = { factor = 2 trait = diligent } break = yes }"
                     $ Random 20 [Modifier 0.5 [Trait "sloth"], Modifier 2 [Trait "diligent"]] [Break]
                   , successTest "If"
                     "if = { limit = { trait = humble prestige = 5 } change_diplomacy = 2 add_trait = monk}"
                     $ If [Trait "humble", Condition (Predicate "prestige") $ NumValue 5] [Concrete "change_diplomacy" $ NumValue 2, AddTrait "monk"]
                   ]

makeCommand :: Text -> Either Error Command
makeCommand s = case statefulParseOnly value (initialPos "test_data") s of
  Left _ -> Left ("",Nothing)
  Right s -> runMaker command s

successTest :: TestName -> Text -> Command -> TestTree
successTest name command result = testCase name $ makeCommand command @?= Right result
