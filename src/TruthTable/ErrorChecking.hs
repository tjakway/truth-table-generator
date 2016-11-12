{-# LANGUAGE ScopedTypeVariables #-}
module TruthTable.ErrorChecking 
( ErrorType(..),
  validateTruthTable
  )
where

import qualified Data.Map.Strict as Map 
import Data.List (nub)
import TruthTable.Types

data ErrorType = VariableLengthError [TruthSet]
                    -- ^ all failing TruthSets
               | NonuniqueTruthValueError [TruthSet]
                    -- ^ all nonunique TruthSets
               | WrongNumberOfTruthSetsError { actual :: Int, expected :: Int }
                    deriving (Show)

validateTruthTable :: TruthTable -> Either ErrorType TruthTable
validateTruthTable a = 
        checkVariableLength a >>= 
        checkNonuniqueTruthSet >>= 
        checkWrongNumberOfTruthSets


checkVariableLength :: TruthTable -> Either ErrorType TruthTable
checkVariableLength truthTable = 
        case checkVariableLength' truthTable of
            [] -> Right truthTable
            failingTruthSets  -> Left $ VariableLengthError failingTruthSets

    where checkVariableLength' :: TruthTable -> [TruthSet]
          checkVariableLength' truthTable' = 
            let numVariables = length . variables $ truthTable' in
            reverse .
            foldr (\thisTruthSet failingSets -> if Map.size thisTruthSet /= numVariables 
                    then thisTruthSet : failingSets
                    else failingSets ) [] 
                    . truthSets $ truthTable'


checkNonuniqueTruthSet :: TruthTable -> Either ErrorType TruthTable
checkNonuniqueTruthSet truthTable
                        | nub tSets == tSets = Right truthTable
                        | otherwise = Left $ NonuniqueTruthValueError tSets
    where tSets = truthSets truthTable

checkWrongNumberOfTruthSets :: TruthTable -> Either ErrorType TruthTable
checkWrongNumberOfTruthSets truthTable 
                        | actualNumTruthSets == expectedNumTruthSets = Right truthTable
                        | otherwise = Left $ WrongNumberOfTruthSetsError actualNumTruthSets expectedNumTruthSets
    where actualNumTruthSets   = length. truthSets $ truthTable
          expectedNumTruthSets = (2 ^) . length . variables $ truthTable


showError :: ErrorType -> String
showError = show -- XXX: implement
