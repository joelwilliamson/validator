module Tests.Trait where

import Trait
import Modifier

import Tests.QuickMaker(quickMake)

import Test.Tasty
import Test.Tasty.HUnit

traitTests =
  testGroup "Trait tests"
  [ testCase "A simple personality trait"
    $ makeTrait "selfish = { personality = yes }"
    @?= Right (defaultTrait { trait_name = "selfish", personality = True })
  , testCase "A trait with more conditions"
    $ makeTrait "vampire = { birth = 1 cannot_marry = yes immortal = yes religious = yes }"
    @?= Right (defaultTrait { trait_name = "vampire", birth = 1, cannot_marry = True
                            , immortal = True, religious = True })
  , testCase "A test with modifiers"
    $ makeTrait "brainiac = { learning = 10 birth = 0 }"
    @?= Right (defaultTrait { trait_name = "brainiac", modifiers = [Modifier "learning" 10]})
  ]

makeTrait = quickMake trait
