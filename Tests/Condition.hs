module Tests.Condition where

import Condition
import Tests.QuickMaker(quickMake)


import Test.Tasty(testGroup)
import Test.Tasty.HUnit


conditionTests = testGroup "Condition Unit Tests"
                  [ testCase "A simple predicate condition"
                    $ makeCondition "is_ruler = yes" @?= Right (BooleanCondition "is_ruler" True)
                  , testCase "A numeric predicate"
                    $ makeCondition "yearly_income = 75" @?= Right (NumericCondition "yearly_income" 75)
                  , testCase "And operator"
                    $ makeCondition "AND = { yearly_income = 50 wealth = 200 }"
                    @?= Right (And [NumericCondition "yearly_income" 50, NumericCondition "wealth" 200])
                  , testCase "Boolean true"
                    $ makeCondition "rebel = yes"
                    @?= Right (BooleanCondition "rebel" True)
                  , testCase "Boolean false"
                    $ makeCondition "is_plot_active = no"
                    @?= Right (BooleanCondition "is_plot_active" False)
                  , testCase "Province id - numeric"
                    $ makeCondition "province_id = 1107"
                    @?= Right (ScopedOrNumeric "province_id" (Left 1107))
                  , testCase "Province id - scope"
                    $ makeCondition "province_id = PREV"
                    @?= Right (ScopedOrNumeric "province_id" (Right Prev))
                  , testCase "Is capital - boolean"
                    $ makeCondition "is_capital = yes"
                    @?= Right (ScopedOrBoolean "is_capital" (Left True))
                  , testCase "Is capital - scope"
                    $ makeCondition "is_capital = ROOT"
                    @?= Right (ScopedOrBoolean "is_capital" (Right Root))
                  , testCase "Conquest culture - boolean"
                    $ makeCondition "conquest_culture = yes"
                    @?= Right (ScopedOrBoolean "conquest_culture" (Left True))
                  , testCase "Conquest culture - scope"
                    $ makeCondition "conquest_culture = ROOT"
                    @?= Right (ScopedOrBoolean "conquest_culture" (Right Root))
                  ]

scopeTests = testGroup "Scope unit tests"
             [
               testCase "Simple limited scope"
               $ makeScope "any_realm_character = { limit = { wealth = 75 } age = 50 }"
               @?= Right (Scope { scopeType_ = (CharacterScope "any_realm_character")
                          , limit = [NumericCondition "wealth" 75]
                          , content = [NumericCondition "age" 50]})
             ]

makeCondition = quickMake condition
makeScope = quickMake (scope condition)
