module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import qualified TruthTable.Tests.ParserTests as ParserTests 
import qualified TruthTable.Tests.LogicTests  as LogicTests

main :: IO ()
main = defaultMainWithOpts 
            [ParserTests.tests, LogicTests.tests]
            mempty
