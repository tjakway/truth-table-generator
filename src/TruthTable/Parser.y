{
module TruthTable.Parser where
import TruthTable.Types
import Data.Char (isAlpha, isSpace)
}




%name parseGrammar
%tokentype { Token }
%error { parseError }
%token
   and  { TokenAnd }
   or   { TokenOr }
   xor  { TokenXor }
   And  { TokenAnd }
   Or   { TokenOr }
   Xor  { TokenXor }
   '-'  { TokenNegation }
   '('  { TokenOB }
   ')'  { TokenCB }
   var  { TokenVar $$ }
   
%%

PStatement : '-' PStatement       { NegationStatement $2 }
          | var                   { VariableStatement (Variable $1) }
          | '(' PStatement ')'    { NestedStatement $2 }
          | PStatement POperator PStatement       { Statement $1 $2 $3 }
          

POperator : and           { And }
         | or            { Or  }
         | xor           { Xor }


{

parseError :: [Token] -> a
parseError _ = error ("Parse error\n")

data Token = 
    TokenAnd
    | TokenOr
    | TokenXor
    | TokenOB
    | TokenCB
    | TokenNegation
    | TokenVar String


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexWord (c:cs)
lexer ('-':cs) = TokenNegation : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

-- | if it's a string of letters it's either a keyword or a variable
lexWord cs = case span isAlpha cs of
                ("and", rest) -> TokenAnd : lexer rest
                ("or", rest) -> TokenOr : lexer rest
                ("xor", rest) -> TokenXor : lexer rest
                ("And", rest) -> TokenAnd : lexer rest
                ("Or", rest) -> TokenOr : lexer rest
                ("Xor", rest) -> TokenXor : lexer rest
                (var, rest) -> TokenVar var : lexer rest

parse :: String -> Statement
parse = parseGrammar . lexer

}
