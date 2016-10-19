module Main where

import TruthTable.Types
import TruthTable.OneOf
import TruthTable.Mapping
import TruthTable.Parser
import Data.List

main :: IO ()
main = getArgs >>= return . intercalate " " >>= print . parseGrammar . lexer
