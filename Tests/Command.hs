module Tests.Command where

import AttoScoped(statefulParseOnly,value)
import Condition(Clause(..),Value(..))
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
                   ]
  where makeCommand :: Text -> Either Error Command
        makeCommand s = case statefulParseOnly value (initialPos "test_data") s of
          Left _ -> Left ("",Nothing)
          Right s -> runMaker command s
