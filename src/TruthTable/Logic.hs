{-# LANGUAGE ScopedTypeVariables #-}
module TruthTable.Logic where

import TruthTable.Types
import TruthTable.Mapping
import Data.List (nub)
import qualified Data.Map.Strict as Map 

uniqueVariables :: Statement -> [Variable]
uniqueVariables = nub . vars
    where vars (StatementResult _) = []
          vars (NestedStatement s) = vars s
          vars (NegationStatement s) = vars s
          vars (VariableStatement v) = [v]
          vars (Statement s1 _ s2) = (vars s1) ++ (vars s2)

getTruthSets :: Statement -> [TruthSet]
getTruthSets = binaryToMap . uniqueVariables


lookupEither :: Ord k => k -> Map.Map k v -> Either k v
lookupEither k m = case Map.lookup k m of Just r  -> Right r
                                          Nothing -> Left k

-- | TODO: Better error handling: Change return type to 
-- Either (TruthSet, Variable) Bool so we have context for the bad input
-- (can say "could not find Variable in TruthSet")
evaluateStatement :: TruthSet -> Statement -> Either Variable Bool
evaluateStatement vars s = 
        let eval = evaluateStatement vars in
            -- ^ evaluate in the context of the current set of truth values
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

catEithers :: [Either a b] -> Either [a] [b]
catEithers = r . foldr f (Right [])
    where f (Left x) (Left cs) = Left $ x : cs
          f (Left x) (Right _) = Left [x]
          f (Right x) (Right ds) = Right $ x : ds
          f (Right _) cs = cs

          r (Left xs) = Left . reverse $ xs
          r (Right xs) = Right . reverse $ xs

-- | TODO: should probably change Either [Variable] TruthTable to 
-- Either [TruthSet] TruthTable so we can see all of the problematic input
-- instead of just getting a list of bad variables with no context
genTruthTable :: Statement -> Either [Variable] TruthTable
genTruthTable stmt = case genTruthTable' of Left vs -> Left vs
                                            Right res -> Right . (uncurry $ TruthTable vars) . unzip $ res

        where 
              genTruthTable' :: Either [Variable] [(TruthSet, Bool)]
              genTruthTable' = catEithers allResults 

              vars :: [Variable]
              vars = uniqueVariables stmt

              inputTruthSets :: [TruthSet]
              inputTruthSets = binaryToMap vars

              allResults :: [Either Variable (TruthSet, Bool)]
              allResults = map (\thisTruthSet -> 
                               evaluateStatement thisTruthSet stmt >>= \r -> return (thisTruthSet, r)) inputTruthSets
