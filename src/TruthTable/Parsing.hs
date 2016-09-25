module TruthTable.Parsing where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Char (toUpper)
import TruthTable.Types

--use regularParse


forbidNumbers :: Parser Char
forbidNumbers = noneOf . mconcat . map show $ [0..9]

-- |returns Just (numLeftParentheses, numRightParentheses) on error,
-- Nothing on success
countParentheses :: String -> Maybe (Int, Int)
countParentheses str = if numLeftParentheses == numRightParentheses 
                           then Nothing 
                           else Just (numLeftParentheses, numRightParentheses)
    where countChar x = foldr (\thisChar counter -> if thisChar == x then counter + 1 else counter) 0 
          numLeftParentheses = countChar '(' str
          numRightParentheses = countChar ')' str

--generic enum parser

--see http://stackoverflow.com/questions/25215184/compact-way-to-map-strings-to-datatype-using-parsec
parseEnumValue :: (Show a) => a -> Parser a
parseEnumValue val = string (map toUpper $ show val) >> return val

parseEnum :: (Show a, Enum a, Bounded a) => Parser a
parseEnum = choice $ map parseEnumValue [minBound..maxBound]

parseOperator :: Parser Operator
parseOperator = parseEnum

-- | a variable is just a single letter
parseVariable :: Parser Variable
parseVariable = letter >>= return . Variable . return

