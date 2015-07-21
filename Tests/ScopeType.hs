module Tests.ScopeType(
  scopeTypeTests
  ) where

import ScopeType

import Tests.QuickMaker(quickMake)

import Test.Tasty
import Test.Tasty.HUnit



scopeTypeTests = testGroup "Scope Type tests" [
  testCase "Simple - All caps"
  $ makeScope "PREVPREV = { break = yes }" @?= Right PrevPrev
  , testCase "Simple - Mixed caps"
    $ makeScope "From = { break = yes }" @?= Right From
  , testCase "Event target"
    $ makeScope "event_target:target = { break = yes }" @?= Right (EventTarget "target")
  , testCase "Empty event target" -- I'm not sure this is the correct behaviour. We probably want to error on empty target.
    $ makeScope "event_target: = { break = yes }" @?= Right (EventTarget "")
  , testCase "Character scope"
    $ makeScope "job_spiritual = { break = yes }" @?= Right (CharacterScope "job_spiritual")
  , testCase "Title scope"
    $ makeScope "e_persia = { break = yes }" @?= Right (IdScope "e_persia")
  , testCase "Concrete title scope"
    $ makeScope "primary_title = e_britannia" @?= Right (TitleScope "primary_title")
  , testCase "Province scope"
    $ makeScope "1103 = { break = yes }" @?= Right (NumScope 1103)
  ]

makeScope = quickMake scopeType
