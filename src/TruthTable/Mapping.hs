module TruthTable.Mapping where

import TruthTable.Types
import qualified Data.Map.Strict as Map 

import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Data.List (nub)

numIterations :: [Variable] -> Int
numIterations = (2 ^) . length 

--see http://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
showAsBinary :: Int -> String
showAsBinary num = showIntAtBase 2 intToDigit num ""

binaryToBool :: Int -> Bool
binaryToBool 0 = False
binaryToBool 1 = True
binaryToBool x = error $ "non-binary value " ++ (show x) ++ " passed to binaryToBool!"

readBinaryChar :: Char -> Bool
readBinaryChar = binaryToBool . read . return

padBinaryStrings :: [String] -> [String]
padBinaryStrings strs = map (\thisString -> 
                            let lengthDiff = maxStrLen - (length thisString)
                                in if lengthDiff > 0 then (take lengthDiff zeros) ++ thisString
                                                     else thisString) strs
        where maxStrLen = maximum . map length $ strs
              zeros     = repeat '0'

binaryToMap :: [Variable] -> [Map.Map Variable Bool]
binaryToMap vars = map (Map.fromList . zip uniqVars . map readBinaryChar) truthBinaryValues
         -- ^ apply return before read because read expects [Char] not Char
         -- even though we're only reading one character (which ought to be
         -- a 0 or 1)
    where uniqVars          = nub vars
          numVars           = numIterations uniqVars
          truthBinaryValues = padBinaryStrings . map showAsBinary $ (reverse [0..(numVars - 1)])
          -- ^ truth values from largest -> smallest
          -- there are 2^(number of variables) iterations but we count from
          -- 0 -> (number of variables - 1)
          -- e.g. 8 in binary is 1000
          -- but it goes 000 -> 111 (which is 7)
          -- this is the same reason that 0 -> 10 is actually 11 numbers
