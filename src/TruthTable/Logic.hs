{-# LANGUAGE ScopedTypeVariables #-}

module TruthTable.Logic where

import TruthTable.OneOf
import TruthTable.Types
import qualified Data.Map.Strict as Map 


evaluateStatement :: Map.Map Variable Bool -> Statement -> Either Variable Bool
evaluateStatement vars (NestedStatement stmt) = evaluateStatement vars stmt
evaluateStatement vars (NegationStatement stmt) = not . evaluateStatement vars $ stmt
evaluateStatement vars (VariableStatement thisVar) = lookupEither vars thisVar
-- ^ if this statement just wraps a variable, just look up its value in the table
evaluateStatement vars (Statement (NestedStatement stmt) op _) = evaluateStatement vars stmt
evaluateStatement vars (Statement _ op (NestedStatement stmt)) = evaluateStatement vars stmt
evaluateStatement vars (Statement (VariableStatement first) op (VariableStatement second)) = 
        let find = lookupOrFail vars in
            evaluateOperator op (find first) (find second)

    where lookupEither :: Ord k => k -> Map.Map k v -> Either k v
          lookupEither k m = case Map.lookup k m of Just r  -> Right r
                                                    Nothing -> Left k

evaluateOperator :: Operator -> Bool -> Bool -> Bool
evaluateOperator op first second 
                    | op == And = first && second
                    | op == Or  = first || second
                    | op == Xor = (first || second) && (first /= second)
