module TruthTable.Entry where

import TruthTable.Logic
import TruthTable.Parser
import TruthTable.Printing
import System.Environment
import Data.List

entry :: IO ()
entry = getArgs >>= return . intercalate " " >>= 
        putStrLn . printResultOrErrorWithDefaultConfig . 
            genTruthTable . parseGrammar . lexer
