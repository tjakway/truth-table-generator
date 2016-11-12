module TruthTable.Tests.LogicTests (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import TruthTable.Types
import TruthTable.Logic
import TruthTable.Parser (parse)
import qualified Data.Map.Strict as Map 

parseEval :: String -> Map.Map Variable Bool -> Either Variable Bool
parseEval input vars = evaluateStatement vars . parse $ input


basicAndTest :: Assertion
basicAndTest = assertEqual "T And T == True" (parseEval "x And y" vars) (Right True)
    where vars = Map.fromList [(Variable "x", True), (Variable "y", True)]

basicOrTrueFalseTest :: Assertion
basicOrTrueFalseTest = assertEqual "T Or F == True" (parseEval "x Or y" vars) (Right True)
    where vars = Map.fromList [(Variable "x", True), (Variable "y", False)]

basicOrFalseTrueTest :: Assertion
basicOrFalseTrueTest = assertEqual "F Or T == True" (parseEval "x Or y" vars) (Right True)
    where vars = Map.fromList [(Variable "x", False), (Variable "y", True)]


fullAndTest :: Assertion
fullAndTest = assertEqual "x And y yields all expected values"
                actualTruthTable expectedTruthTable
    where stmt = parse "x And y"
          expectedVariables = [Variable "x", Variable "y"]
          expectedTruthSets = fmap Map.fromList
                                [ [(Variable "x", True), (Variable "y", True)],
                                    [(Variable "x", True), (Variable "y", False)],
                                    [(Variable "x", False), (Variable "y", True)],
                                    [(Variable "x", False), (Variable "y", False)] ]

          expectedResults = [True, False, False, False]

          expectedTruthTable = Right $ TruthTable expectedVariables expectedTruthSets expectedResults

          actualTruthTable = genTruthTable stmt
          


tests = testGroup "LogicTests" [testCase "basicAndTest" basicAndTest,
                                testCase "basicOrTrueFalseTest" basicOrTrueFalseTest,
                                testCase "basicOrFalseTrueTest" basicOrFalseTrueTest,
                                testCase "fullAndTest" fullAndTest]
