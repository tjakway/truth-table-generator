module TruthTable.Tests.BasicTests (tests) where

import Test.HUnit

basicTest :: Assertion 
basicTest = return ()

tests = testGroup "BasicTests" [testCase "basicTest" basicTest]
