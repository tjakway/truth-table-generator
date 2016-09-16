module TruthTable.Mapping where

import TruthTable.Types
import qualified Data.Map.Strict as Map 

import Data.Char (intToDigit)
import Numeric (showIntAtBase)

numIterations :: [Variable] -> Integer
numIterations = (2 ^) . length 

--see http://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
showAsBinary :: Int -> String
showAsBinary num = showIntAtBase 2 intToDigit num ""

binaryToBool :: Int -> Bool
binaryToBool 0 = False
binaryToBool 1 = True
binaryToBool x = error $ "non-binary value " ++ (show x) ++ " passed to binaryToBool!"

binaryToMap :: [Variable] -> Int -> Map.Map Variable Bool
binaryToMap vars = Map.fromList . zip vars . map (binaryToBool . read) . showAsBinary
