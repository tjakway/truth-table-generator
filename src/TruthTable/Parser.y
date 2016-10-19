{
module TruthTable.Parser where
import TruthTable.Types
}

%name parseGrammar
%tokentype { Token }
%error { parseError }

%%

%token
   and  { TokenAnd }
   or   { TokenOr }
   xor  { TokenXor }
   '-'  { TokenNegation }
   '('  { TokenOB }
   ')'  { TokenCB }
   var  { TokenVar }
   
{
happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Token = 
    TokenAnd
    | TokenOr
    | TokenXor
    | TokenOB
    | TokenCB
    | TokenVar


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

-- XXX: REWRITE HAPPY-STYLE
operator ::= and | or | xor           { Operator $1 }
statement ::=  '-' statement         { Negation $2 }
                | var                   { VariableStatement $3 }
                | statement                 { NestedStatement $1 }

program ::= statement  
