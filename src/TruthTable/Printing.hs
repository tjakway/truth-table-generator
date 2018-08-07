module TruthTable.Printing
(PrintConfig(..),
printM,
printTruthTable,
printWithDefaultConfig,
printResultOrErrorWithDefaultConfig
)
where

import TruthTable.Types
-- there's no reason to be Lazy when printing (we discard results right
-- after anyway) and String generates a lot of thunks
import Control.Monad.State.Strict

data PrintConfig = PrintConfig {
                 delimiter :: String,
                 trueString :: String,
                 falseString :: String,
                 resultColumnName :: String
                 }

type Printer = State (PrintConfig, TruthTable)

defaultConfig :: PrintConfig
defaultConfig = PrintConfig { delimiter= "\t", trueString = "T", falseString = "F", resultColumnName = "Result" }

-- | take a row from the TruthTable, return it, and update state to reflect
-- this
takeRow :: Printer (TruthSet, Bool)
takeRow = do
        (conf, truthTable) <- get
        let split [] = error "Cannot split an empty list"
            split (x:xs) = (x, xs)

        (thisTruthSet, remTruthSets) <- split . truthSets <$> getTruthTable
        (thisResult, remResults) <- split . rs <$> getTruthTable

        let newTruthTable = truthTable { 
                                truthSets = remTruthSets,
                                rs = remResults }

        put (conf, newTruthTable)
        return (thisTruthSet, thisResult)

getConfig :: Printer PrintConfig
getConfig = fst <$> get

getTruthTable :: Printer TruthTable
getTruthTable = snd <$> get

getVariables :: Printer [Variable]
getVariables = variables . snd <$> get

printBool :: PrintConfig -> Bool -> String
printBool conf True = trueString conf
printBool conf False = falseString conf

printRow :: Printer String
printRow = do
        truthTable <- getTruthTable
        -- make sure a row exists before we take one
        if (not . null . truthSets $ truthTable) && (not . null . rs $ truthTable)
            then printRow'
            --if there are no rows left to print return an empty string
            else return ""
    where printRow' = do
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
        let header = foldr (\(Variable varName) acc -> acc ++ delim ++ varName) "" vars
            resColName = resultColumnName conf
        
        -- have to add the result column header explicitly
        return $ header ++ delim ++ resColName
        

printRows :: Printer (Either String String)
printRows = do
        truthTable <- getTruthTable
        let truthSetsLength = length . truthSets $ truthTable
            resultsLength = length . rs $ truthTable
        
        printedRow <- printRow

        if truthSetsLength /= resultsLength
            then return . Left $ 
                        "Number of TruthSets does not match number of results! " ++ 
                        (show . truthSets $ truthTable) ++
                        (show . rs $ truthTable)
            else if (truthSetsLength == 0) && (resultsLength == 0)
                     -- if we're out of rows to print we're done
                     then return . Right $ printedRow
                     -- recurse to print the next row
                     -- we have to bind twice: once to unwrap the state
                     -- monad, again to unwrap the Either type
                     else (fmap ((printedRow ++ "\n") ++)) <$> printRows


printM :: Printer (Either String String)
printM = do
        header <- printHeader
        rowsR <- printRows
        --prepend the header if the message is valid
        case rowsR of (Left e) -> return . Left $ e
                      (Right printedRows) -> return . Right $ header ++ "\n" ++ printedRows

printTruthTable :: PrintConfig -> TruthTable -> Either String String
printTruthTable conf truthTable = evalState printM (conf, truthTable)

printWithDefaultConfig :: TruthTable -> Either String String
printWithDefaultConfig = printTruthTable defaultConfig

printResultOrErrorWithDefaultConfig :: Either [Variable] TruthTable -> String
printResultOrErrorWithDefaultConfig (Left vars) = "Error: " ++ (show vars)
printResultOrErrorWithDefaultConfig (Right truthTable) = 
        case printWithDefaultConfig truthTable of
                Right result -> result
                Left err -> "Error: " ++ err

