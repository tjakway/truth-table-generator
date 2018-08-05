module Main where

import TruthTable.Types
import TruthTable.Mapping
import TruthTable.Parser
import TruthTable.Printing
import System.Environment
import Data.List

-- TODO: handle no args
main :: IO ()
main = getArgs >>= return . intercalate " " >>= 
        putStrLn . printResultOrErrorWithDefaultConfig . 
            genTruthTable . parseGrammar . lexer
