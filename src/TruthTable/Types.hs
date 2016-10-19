module TruthTable.Types where

import TruthTable.OneOf
import qualified Data.Map.Strict as Map 


-- | lookup or die with an informative error
-- basically fromJust with a bad excuse for diagnostics
lookupOrFail mapping item  = case Map.lookup item mapping of 
                                      Just x  -> x
                                      Nothing -> error ("Lookup of " ++ (show item) ++ " in " ++ (show mapping) ++ " failed!")


data Operator = And
              | Or
              | Xor
              deriving (Eq, Ord, Show, Read, Bounded, Enum)

newtype Variable = Variable String
                   deriving (Eq, Ord, Show)

data Statement = NestedStatement Statement
               | NegationStatement Statement
               | VariableStatement Variable
               -- ^ a statement that just wraps a variable so we can negate
               -- variables without further work
               | Statement Statement Operator Statement
                deriving (Eq, Show)

evaluateStatement :: Map.Map Variable Bool -> Statement -> Bool
evaluateStatement vars (NestedStatement stmt) = evaluateStatement vars stmt
evaluateStatement vars (NegationStatement stmt) = not . evaluateStatement vars $ stmt
evaluateStatement vars (VariableStatement thisVar) = lookupOrFail vars thisVar
-- ^ if this statement just wraps a variable, just look up its value in the table
evaluateStatement vars (Statement (NestedStatement stmt) op _) = evaluateStatement vars stmt
evaluateStatement vars (Statement _ op (NestedStatement stmt)) = evaluateStatement vars stmt
evaluateStatement vars (Statement (VariableStatement first) op (VariableStatement second)) = 
        let find = lookupOrFail vars in
            evaluateOperator op (find first) (find second)

evaluateOperator :: Operator -> Bool -> Bool -> Bool
evaluateOperator op first second 
                    | op == And = first && second
                    | op == Or  = first || second
                    | op == Xor = (first || second) && (first /= second)
