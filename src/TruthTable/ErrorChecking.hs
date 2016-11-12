{-# LANGUAGE ScopedTypeVariables #-}
module TruthTable.ErrorChecking 
( ErrorType(..),
  validateTruthTable
  )
where

import qualified Data.Map.Strict as Map 
import TruthTable.Types

data ErrorType = VariableLengthError [TruthSet]
                    -- ^ all failing TruthSets
               | NonuniqueTruthValueError [TruthSet]
                    -- ^ all nonunique TruthSets
               | WrongNumberOfTruthSetsError Int Int
                    -- ^ Expected and Actual
                    deriving (Show)

validateTruthTable :: TruthTable -> Either ErrorType TruthTable
validateTruthTable truthTable = undefined
    where numVariables :: Int
          numVariables = length . variables $ truthTable


eitherCheckVariableLengthError :: TruthTable -> Either ErrorType TruthTable
eitherCheckVariableLengthError truthTable = 
        case checkVariableLengthError truthTable of
            [] -> Right truthTable
            failingTruthSets  -> Left $ VariableLengthError failingTruthSets

checkVariableLengthError :: TruthTable -> [TruthSet]
checkVariableLengthError truthTable = 
        reverse .
        foldr (\thisTruthSet failingSets -> if Map.size thisTruthSet /= numVariables 
            then thisTruthSet : failingSets
            else failingSets ) [] 
            . truthSets $ truthTable

    where numVariables :: Int
          numVariables = length . variables $ truthTable

showError :: ErrorType -> String
showError = show -- XXX: implement
