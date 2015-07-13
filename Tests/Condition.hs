module Tests.Condition where

import Condition
import Tests.QuickMaker(quickMake)


import Test.Tasty(testGroup)
import Test.Tasty.HUnit


conditionTests = testGroup "Condition Unit Tests"
                  [ testCase "A simple predicate condition"
                    $ makeCondition "is_ruler = yes" @?= Right (Condition (Predicate "is_ruler") (BooleanValue True))
                  , testCase "A numeric predicate"
                    $ makeCondition "yearly_income = 75" @?= Right (Condition (Predicate "yearly_income") (NumValue 75))
                  , testCase "And operator"
                    $ makeCondition "AND = { yearly_income = 50 wealth = 200 }"
                    @?= Right (And [Condition "yearly_income" (NumValue 50), Condition "wealth" (NumValue 200)])
                  , testCase "Or operator"
                    $ makeCondition "OR = { de_jure_liege = PREV de_jure_liege = PREVPREV }"
                    @?= Right (Or [Condition "de_jure_liege" (ScopedValue Prev)
                                  ,Condition "de_jure_liege" (ScopedValue PrevPrev)])
                  ]

makeCondition = quickMake condition
