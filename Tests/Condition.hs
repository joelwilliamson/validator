module Tests.Condition where

import Condition
import Tests.QuickMaker(quickMake)

import qualified Data.Set as S


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
                  , testCase "Counted scope"
                    $ makeCondition "any_courtier = { trait = brave count = 4 }"
                    @?= Right (Scoped $ Scope { scopeType_ = CharacterScope "any_courtier"
                                              , limit = mempty
                                              , content = [Trait "brave", NumericCondition "count" 4]})
                  , testCase "Nor block"
                    $ makeCondition "NOR = { religion = catholic culture = norman }"
                    @?= Right (Not [Condition "religion" $ Id "catholic", Condition "culture" $ Id "norman"])
                  , testCase "Nand block"
                    $ makeCondition "NAND = { wealth = 50 age = 16 }"
                    @?= Right (Nand [NumericCondition "wealth" 50, NumericCondition "age" 16])
                  , testCase "Compare in other scope"
                    $ makeCondition "num_of_trade_post_diff = { character = FROMFROM value = 10 }"
                    @?= Right (Scoped (Scope FromFrom mempty [NumericCondition "num_of_trade_post_diff" 10]))
                  , testCase "Religion Authority can take a scope as argument"
                    $ makeCondition "religion_authority = ROOT"
                    @?= Right (ScopedOrNumeric "religion_authority" (Right Root))
                  , testCase "Variable check"
                    $ makeCondition "is_variable_equal = { which = v1 value = 10 }"
                    @?= Right (VariableCheck "v1" (Right 10))
                  ]

scopeTests = testGroup "Scope unit tests"
             [
               testCase "Simple limited scope"
               $ makeScope "any_realm_character = { limit = { wealth = 75 } age = 50 }"
               @?= Right (Scope { scopeType_ = (CharacterScope "any_realm_character")
                          , limit = S.fromList [NumericCondition "wealth" 75]
                          , content = [NumericCondition "age" 50]})
             , testCase "No limit scpope"
               $ makeScope "random_vassal = { age = 15 }"
               @?= Right (Scope { scopeType_ = CharacterScope "random_vassal"
                                , limit = mempty
                                , content = [NumericCondition "age" 15]})
             , testCase "Two limit clauses"
               $ makeScope "random_courtier = { limit = { age = 15 } limit = { trait = wroth } martial = 20 }"
               @?= Right (Scope { scopeType_ = CharacterScope "random_courtier"
                                , limit = S.fromList [NumericCondition "age" 15
                                                   , Trait "wroth"]
                                , content = [NumericCondition "martial" 20] })
             , testCase "Disordered limit clauses"
               $ makeScope "any_realm_title = { limit = { claimed_by = PREV conquest_culture = yes } is_occupied = no }"
               @?= Right (Scope { scopeType_ = TitleScope "any_realm_title"
                                , limit = S.fromList [ScopedOrBoolean "conquest_culture" (Left True)
                                                     , Condition "claimed_by" (Id "PREV")]
                                , content = [BooleanCondition "is_occupied" False]})
             ]

makeCondition = quickMake condition
makeScope = quickMake (scope condition)
