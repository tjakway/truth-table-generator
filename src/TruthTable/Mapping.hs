module TruthTable.Mapping where

import TruthTable.Types
import qualified Data.Map.Strict as Map 

import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Data.List (nub)

numIterations :: [Variable] -> Integer
numIterations = (2 ^) . length 

--see http://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
showAsBinary :: Int -> String
showAsBinary num = showIntAtBase 2 intToDigit num ""

binaryToBool :: Int -> Bool
binaryToBool 0 = False
binaryToBool 1 = True
binaryToBool x = error $ "non-binary value " ++ (show x) ++ " passed to binaryToBool!"

binaryToMap :: [Variable] -> [Map.Map Variable Bool]
binaryToMap vars = map (Map.fromList . zip uniqVars . map (binaryToBool . read . return)) truthBinaryValues
         -- ^ apply return before read because read expects [Char] not Char
         -- even though we're only reading one character (which ought to be
         -- a 0 or 1)
    where uniqVars          = nub vars
          numVars           = length uniqVars
          truthBinaryValues = map showAsBinary (reverse [0..numVars])
          -- ^ truth values from largest -> smallest
