module TruthTable.Tests.LogicTests (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import TruthTable.Types
import TruthTable.Logic
import qualified TruthTable.Parser as Parser
import qualified Data.Map.Strict as Map 

parseEval :: String -> Map.Map Variable Bool -> Either Variable Bool
parseEval input vars = evaluateStatement vars . Parser.parse $ input

basicAndTest :: Assertion
basicAndTest = assertEqual "T And T == True" (parseEval "x And y" vars) (Right True)
    where vars = Map.fromList [(Variable "x", True), (Variable "y", True)]

tests = testGroup "LogicTests" [testCase "basicAndTest" basicAndTest]
