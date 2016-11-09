module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import qualified TruthTable.Tests.ParserTests as ParserTests 

main :: IO ()
main = defaultMainWithOpts 
            [ParserTests.tests]
            mempty
