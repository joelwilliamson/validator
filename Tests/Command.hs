module Tests.Command where

import AttoScoped(statefulParseOnly,value)
import Condition(Clause(..),Value(..),ScopeType(..))
import Command
import Maker(runMaker)
import Scoped(Error())

import Data.Attoparsec.ByteString
import Text.Parsec.Pos(initialPos)
import Data.Text(Text)

import Test.Tasty
import Test.Tasty.HUnit

commandUnitTests = testGroup "Command Unit Tests"
                   [ testCase "A simple concrete command" $
                     makeCommand "abandon_heresy = yes" @?= Right (Concrete "abandon_heresy" (BooleanValue True))
                   , testCase "Numeric command" $
                     makeCommand "change_diplomacy = 3" @?= Right (Concrete "change_diplomacy" (NumValue 3))
                   , testCase "Adding a trait" $
                     makeCommand "add_trait = diligent" @?= Right (AddTrait "diligent")
                   , testCase "Removing a trait" $
                     makeCommand "remove_trait = wroth" @?= Right (RemoveTrait "wroth")
                   , testCase "Set Character Flag" $
                     makeCommand "set_character_flag = char_flag" @?= Right (SetFlag Character "char_flag")
                   , testCase "Clear Province Flag" $
                     makeCommand "clr_province_flag = prov_flag" @?= Right (ClrFlag Province "prov_flag")
                   , testCase "Add two variables" $
                     makeCommand "change_variable = { which = a which = b }" @?= Right (VarOpVar "a" Change "b")
                   , testCase "Subtract constant from variable" $
                     makeCommand "subtract_variable = { which = a value = 4 }" @?= Right (VarOpLit "a" Subtract 4)
                   , testCase "Spawn a unit" $
                     makeCommand "spawn_unit = { province = 342 owner = THIS leader = FROM home = PREV attrition = 1 troops = { archers = { 100 100 } }}"
                     @?= Right (SpawnUnit 342 (Just This) (Just From) (Just Prev)  Nothing (Just 1) (Troops "archers" 100 100))
                   , testCase "Break" $
                     makeCommand "break = yes" @?= Right Break
                   ]
  where makeCommand :: Text -> Either Error Command
        makeCommand s = case statefulParseOnly value (initialPos "test_data") s of
          Left _ -> Left ("",Nothing)
          Right s -> runMaker command s
