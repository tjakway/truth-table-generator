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
printHeader = undefined

printM :: Printer String
printM = undefined
