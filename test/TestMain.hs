module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import qualified TruthTable.Tests.BasicTests as BasicTests

main :: IO ()
main = defaultMainWithOpts 
            [BasicTests.tests, MultiTableTests.tests, integrationTests, aiTests]
            mempty
