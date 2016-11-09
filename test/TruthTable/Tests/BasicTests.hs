module TruthTable.Tests.BasicTests (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

basicTest :: Assertion 
basicTest = return ()

tests = testGroup "BasicTests" [testCase "basicTest" basicTest]
