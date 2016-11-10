module TruthTable.Logic where

import TruthTable.OneOf
import TruthTable.Types
import qualified Data.Map.Strict as Map 

lookupEither :: Ord k => k -> Map.Map k v -> Either k v
lookupEither k m = case Map.lookup k m of Just r  -> Right r
                                          Nothing -> Left k

evaluateStatement :: Map.Map Variable Bool -> Statement -> Either Variable Bool
evaluateStatement vars s = 
        let eval = evaluateStatement vars in
            -- ^ evaluate in the current context
        case s of StatementResult b -> Right b
                  NestedStatement stmt -> eval stmt
                  VariableStatement thisVar -> lookupEither thisVar vars
                  NegationStatement stmt -> fmap not $ eval stmt
                  Statement s1 op s2 -> do
                      let fOp = evaluateOperator op
                      firstResult <- eval s1
                      secondResult <- eval s2
                      return $ fOp firstResult secondResult

evaluateOperator :: Operator -> Bool -> Bool -> Bool
evaluateOperator And a b = (a && b)
evaluateOperator Or a b = (a || b)
evaluateOperator Xor a b = (a || b) && (a /= b)
