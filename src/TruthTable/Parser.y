{
module TruthTable.Parser where
import TruthTable.Types
}




%name parseGrammar
%tokentype { Token }
%error { parseError }
%token
   and  { TokenAnd }
   or   { TokenOr }
   xor  { TokenXor }
   '-'  { TokenNegation }
   '('  { TokenOB }
   ')'  { TokenCB }
   var  { TokenVar $$ }
   
%%

POperator : and           { And }
         | or            { Or  }
         | xor           { Xor }

PStatement : '-' PStatement         { NegationStatement $2 }
          | PStatement PStatement   { NestedStatement $2 }
          | var                   { VariableStatement $1 }
          | PStatement POperator PStatement       { Statement $1 $2 $3 }

{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

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
                (var, rest) -> TokenVar var : lexer rest

}
