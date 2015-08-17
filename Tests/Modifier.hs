module Tests.Modifier where

import Modifier
import Tests.QuickMaker(quickMake)

import Test.Tasty(testGroup)
import Test.Tasty.HUnit

modifierTests = testGroup "Modifier Unit Tests"
  [ testCase "Diplomacy Modifier"
    $ makeMod "diplomacy = 5"
    @?= Right (Modifier "diplomacy" 5)
  , testCase "Trade value"
    $ makeMod "tradevalue = 10"
    @?= Right (Modifier "tradevalue" 10)
  ]

makeMod = quickMake modifier
