{-# LANGUAGE ScopedTypeVariables #-}
module TruthTable.Logic where

import TruthTable.Types
import TruthTable.Mapping
import Data.Bifunctor
import Data.List (nub)
import qualified Data.Map.Strict as Map 

uniqueVariables :: Statement -> [Variable]
uniqueVariables = nub . variables
    where variables (StatementResult _) = []
          variables (NestedStatement s) = variables s
          variables (NegationStatement s) = variables s
          variables (VariableStatement v) = [v]
          variables (Statement s1 op s2) = (variables s1) ++ (variables s2)

getTruthSets :: Statement -> [TruthSet]
getTruthSets = binaryToMap . uniqueVariables


lookupEither :: Ord k => k -> Map.Map k v -> Either k v
lookupEither k m = case Map.lookup k m of Just r  -> Right r
                                          Nothing -> Left k

evaluateStatement :: TruthSet -> Statement -> Either Variable Bool
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


transformLefts :: (a -> c) -> [Either a b] -> ([c], [b])
transformLefts alt = bimap reverse reverse . foldr f ([], [])
    where f (Left x)  (cs, ds) = ((alt x) : cs, ds)
          f (Right x) (cs, ds) = (cs, x : ds)


catEithers :: [Either a b] -> Either [a] [b]
catEithers = foldr f (Right [])
    where f (Left x) (Left cs) = Left $ x : cs
          f (Left x) (Right _) = Left [x]
          f (Right x) (Right ds) = Right $ x : ds
          f (Right _) cs = cs

genTruthTable :: Statement -> Either [Variable] [(TruthSet, Bool)]
genTruthTable stmt = catEithers allResults

        where inputTruthSets :: [TruthSet]
              inputTruthSets = binaryToMap . uniqueVariables $ stmt
              allResults = map (\thisTruthSet -> 
                               evaluateStatement thisTruthSet stmt >>= \r -> return (thisTruthSet, r)) inputTruthSets
--              allResults = map (\truthSet -> (truthSet, evaluateStatement truthSet stmt)) truthTable
