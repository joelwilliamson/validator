module Tests.Trait where

import Trait

import Tests.QuickMaker(quickMake)

import Test.Tasty
import Test.Tasty.HUnit

traitTests =
  testGroup "Trait tests"
  [ testCase "A simple personality trait"
    $ makeTrait "selfish = { personality = yes }"
    @?= Right (defaultTrait { trait_name = "selfish", personality = True })
  ]

makeTrait = quickMake trait
