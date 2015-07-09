module Tests.AttoScoped (parseTests) where

import AttoScoped

import Tree
import Scoped(Atom(..))
import Data.Text as T
import Text.Parsec.Pos(initialPos)

import Test.Tasty
import Test.Tasty.HUnit

parseTests = testGroup "Parsing Tests"
             [ successTest "Single assignment"
               "left = right" $ Node "left" [Node "right" [] Nothing] Nothing
             , successTest "Block with single inner"
               "outer = { left = right }"
               $ Node "outer" [Node "left" [Node "right" [] Nothing] Nothing] Nothing
             , successTest "Block with several inners"
               "outer = { l1 = r1 l2 = r2 }"
               $ Node "outer" [Node "l1" [Node "r1" [] Nothing] Nothing
                              ,Node "l2" [Node "r2" [] Nothing] Nothing] Nothing
             , successTest "Non-equated block"
               "outer = { 100 100 }"
               $ Node "outer" [Node (Number 100) [Node (Number 100) [] Nothing] Nothing] Nothing
             , successTest "With newline"
               "outer = #end of line comment\n\tinner\n"
               $ Node "outer" [Node "inner" [] Nothing] Nothing
             , testCase "No closing brace"
               $ statefulParseOnly value (initialPos "test") "outer = { left = right"
               @?= Left "Failed reading: takeWhile1"
             , testCase "Only a single element when expecting a pair"
               $ statefulParseOnly value (initialPos "test") "key"
               @?= Left "'=': not enough input"
             , successTest "Quoted string"
               "key = \"value\""
               $ Node "key" [Node "value" [] Nothing] Nothing
             ]

successTest :: String → T.Text → Tree Atom → TestTree
successTest name input result =
  testCase name
  $ assertEqual "Blah" (statefulParseOnly value (initialPos "test data") input) (Right result)
