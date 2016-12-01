module TruthTable.Printing where

import TruthTable.Types
import Control.Monad.State.Lazy

data PrintConfig = PrintConfig {
                 delimiter :: String,
                 trueString :: String,
                 falseString :: String
                 }

type Printer = State (PrintConfig, TruthTable)

-- | take a row from the TruthTable, return it, and update state to reflect
-- this
takeRow :: Printer (TruthSet, Bool)
takeRow = do
        (conf, truthTable) <- get
        let thisTruthSet = head . truthSets $ truthTable
            thisResult   = head . rs $ truthTable
            newTruthTable = truthTable { 
                            truthSets = (init . truthSets $ truthTable),
                                rs = (init . rs $ truthTable) }
        put (conf, newTruthTable)
        return (thisTruthSet, thisResult)

getConfig :: Printer PrintConfig
getConfig = get >>= return . fst

getTruthTable :: Printer TruthTable
getTruthTable = get >>= return . snd

getVariables :: Printer [Variable]
getVariables = get >>= return . variables . snd

printBool :: PrintConfig -> Bool -> String
printBool conf True = trueString conf
printBool conf False = falseString conf

printRow :: Printer String
printRow = do
        conf <- getConfig
        (truthSet, result) <- takeRow

        let pBool = printBool conf
            delim = delimiter conf

        -- TODO: consider printing the first item separately so the line
        -- doesn't start with a delimiter
        let rowStr = foldr (\thisTruthValue acc -> acc ++ delim ++ pBool thisTruthValue) "" truthSet
        
        -- don't forget to print the result as the last column
        return $ rowStr ++ delim ++ pBool result


printHeader :: Printer String
printHeader = do
        conf <- getConfig
        vars <- getVariables

        let delim = delimiter conf

        -- TODO: consider printing the first item separately so the line
        -- doesn't start with a delimiter
        return $ foldr (\(Variable varName) acc -> acc ++ delim ++ varName) "" vars
        

printRows :: Printer (Either String String)
printRows = do
        truthTable <- getTruthTable
        let truthSetsEmpty = null . truthSets $ truthTable
            resultsEmpty = null . rs $ truthTable
        
        printedRow <- printRow

        if truthSetsEmpty && (not resultsEmpty) || (not truthSetsEmpty) && resultsEmpty 
            then return . Left $ "Number of TruthSets does not match number of results! " ++ (show . truthSets $ truthTable) ++
                (show . rs $ truthTable)
            else if truthSetsEmpty && resultsEmpty 
                     -- if we're out of rows to print we're done
                     then return . Right $ printedRow
                     -- recurse to print the next row
                     -- we have to bind twice: once to unwrap the state
                     -- monad, again to unwrap the Either type
                     else printRows >>= (\nextRowR -> return $ nextRowR >>= (\nextRow -> Right $ printedRow ++ "\n" ++ nextRow))
