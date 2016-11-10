module TruthTable.Tests.ParserTests (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import TruthTable.Types
import qualified TruthTable.Parser as Parser

andTest :: Assertion
andTest = assertEqual 
    "The parser can correctly parse x And y"
    (Statement (VariableStatement (Variable "x")) And (VariableStatement (Variable "y")))
    (Parser.parse "x And y")

tests = testGroup "ParserTests" [testCase "andTest" andTest]
