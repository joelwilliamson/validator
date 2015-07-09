module Tests.Maker (makerTests) where

import Maker
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text as T
import Text.Parsec.Pos(initialPos,newPos)
import Scoped(Error)
import AttoScoped(statefulParseOnly,value)

makerTests = testGroup "Maker tests"
             [ testCase "Running a simple maker does not query the tree"
               $ runMaker (return (10:: Integer)) undefined @?= Right 10
             , testCase "Getting the label"
               $ parseMake (label key) "key = value" @?= Right "key"
             , testCase "Getting the value"
               $ parseMake fetchString "key = value" @?= Right "value"
             , testCase "Attempt getting the child of a node with no children"
               $ parseMake (firstChild fetchString) "key = value"
               @?= Left ("Can't take a value at Label \"value\"", Nothing)
             , testCase "Get child with no children, explanatory"
               $ parseMake (firstChild fetchString <?> "Get child string") "key = value"
               @?= Left ("Get child string => Can't take a value at Label \"value\"", Nothing)
             , testCase "Failing get grandchild"
               $ parseMake (firstChild $ firstChild fetchString) "key = value"
               @?= Left ("No children: Label \"value\"",Nothing)
             , testCase "Get every child label"
               $ parseMake (mapSubForest $ label key) "outer = { l1 = 1 l2 = 10 }"
               @?= Right ["l1","l2"]
             , testCase "Single child - succeed"
               $ parseMake singleChild "key = value"
               @?= Right ()
             , testCase "Single child - fail"
               $ parseMake singleChild "key = { l1 = 1 l2 = 2 }"
               @?= Left ("Not one child", Just $ newPos "test" 1 1)
             , testCase "Attempt getting a string when given a number"
               $ parseMake fetchString "key = 10"
               @?= Left ("Expected string for value of Label \"key\", got: 10.0",Just $ newPos "test" 1 1)
             , testCase "Attempt getting a string when given a block"
               $ parseMake fetchString "key = { l1 = r1 l2 = r2 }"
               @?= Left ("Expected a string for value of Label \"key\", got a block.", Just $ newPos "test" 1 1)
             , testCase "Succeeding optional"
               $ parseMake (optional fetchString) "key = value" @?= Right (Just "value")
             , testCase "Failing optional"
               $ parseMake (optional fetchString) "key = 10" @?= Right Nothing
             , testCase "Get the label at a given label"
               $ parseMake (label key @@ "key") "outer = { key = value k2 = v2 }"
               @?= Right "key"
             , testCase "Get a number at given label"
               $ parseMake (number ~@ "key") "outer = { key = 10 fake = 1}" @?= Right 10
             , testCase "Missing key"
               $ parseMake (number ~@ "key") "outer = { k = v }"
               @?= Left ("No child: key in block with root: Label \"outer\"", Just $ newPos "test" 1 1)
             , testCase "Optional value - fail"
               $ parseMake (number ~? "key") "outer = { k = 1 }"
               @?= Right Nothing
             , testCase "Optional value - succeed"
               $ parseMake (number ~? "key") "outer = { key = 1 }"
               @?= Right (Just 1)
             , testCase "checkKey - fail"
               $ parseMake (checkKey "key") "k = v"
               @?= Left ("Check for \"key\" failed. Found: Label \"k\"",Just $ newPos "test" 1 1)
             , testCase "checkKey - succeed"
               $ parseMake (checkKey "key") "key = value"
               @?= Right "key"
             , testCase "checkKey - number"
               $ parseMake (checkKey "key") "10 = value"
               @?= Left ("Check for key \"key\" failed. Found number", Just $ newPos "test" 1 1)
             , testCase "checkKeys - Succeed"
               $ parseMake (checkKeys ["k1","k2"]) "k2 = v2" @?= Right "k2"
             , testCase "checkKeys - Fail"
               $ parseMake (checkKeys ["k1","k2"]) "key = value"
               @?= Left ("Check for keys failed. Found: Label \"key\"", Just $ newPos "test" 1 1)
             ]

parseMake :: Maker a → T.Text → Either Error a
parseMake m t = case statefulParseOnly value (initialPos "test") t of
  Left _ → error "Parse failed in Maker test"
  Right s → runMaker m s
