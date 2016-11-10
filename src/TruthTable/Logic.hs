module TruthTable.Logic where

import TruthTable.OneOf
import TruthTable.Types
import qualified Data.Map.Strict as Map 

lookupEither :: Ord k => k -> Map.Map k v -> Either k v
lookupEither k m = case Map.lookup k m of Just r  -> Right r
                                          Nothing -> Left k

evaluateStatement :: Map.Map Variable Bool -> Statement -> Either Variable Bool
evaluateStatement vars (StatementResult b) = Right b
evaluateStatement vars (NestedStatement stmt) = evaluateStatement vars stmt
evaluateStatement vars (Statement (VariableStatement first) op (VariableStatement second)) = 
        let find x = lookupEither x vars in
            find first >>= \f ->
            find second >>= \s -> 
            return $ evaluateOperator op f s
evaluateStatement vars (NegationStatement stmt) = fmap not $ evaluateStatement vars $ stmt
evaluateStatement vars (VariableStatement thisVar) = lookupEither thisVar vars
-- ^ if this statement just wraps a variable, just look up its value in the table
evaluateStatement vars s = let eval = fmap StatementResult $ evaluateStatement vars in
    case s of (Statement (NestedStatement stmt) op x) -> (eval stmt) >>= \res -> eval (Statement res op x)
              (Statement x op (NestedStatement stmt)) -> (eval stmt) >>= eval $ Statement x op 

evaluateOperator :: Operator -> Bool -> Bool -> Bool
evaluateOperator op first second 
                    | op == And = first && second
                    | op == Or  = first || second
                    | op == Xor = (first || second) && (first /= second)
