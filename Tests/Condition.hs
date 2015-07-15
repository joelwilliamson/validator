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
                  ]

makeCondition = quickMake condition
