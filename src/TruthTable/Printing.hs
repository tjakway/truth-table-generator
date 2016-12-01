module TruthTable.Printing where

import TruthTable.Types
import Control.Monad.State.Lazy

data PrintConfig = PrintConfig {
                 delimiter :: String,
                 trueString :: String,
                 falseString :: String
                 }

type Printer = State (PrintConfig, TruthTable)

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


printRow :: Printer String
printRow = undefined

printHeader :: Printer String
printHeader = undefined


printM :: Printer String
printM = undefined
