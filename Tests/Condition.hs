module Tests.Condition where

import Condition
import Tests.QuickMaker(quickMake)


import Test.Tasty(testGroup)
import Test.Tasty.HUnit


conditionTests = testGroup "Condition Unit Tests"
                  [ testCase "A simple predicate condition"
                    $ makeCondition "is_ruler = yes" @?= Right (BooleanCondition "is_ruler" True)
                  , testCase "A numeric predicate"
                    $ makeCondition "yearly_income = 75" @?= Right (Condition "yearly_income" (NumValue 75))
                  , testCase "And operator"
                    $ makeCondition "AND = { yearly_income = 50 wealth = 200 }"
                    @?= Right (And [Condition "yearly_income" (NumValue 50), Condition "wealth" (NumValue 200)])
                  , testCase "Boolean true"
                    $ makeCondition "rebel = yes"
                    @?= Right (BooleanCondition "rebel" True)
                  , testCase "Boolean false"
                    $ makeCondition "is_plot_active = no"
                    @?= Right (BooleanCondition "is_plot_active" False)
                  ]

makeCondition = quickMake condition
