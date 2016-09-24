module TruthTable.Parsing where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
--
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
