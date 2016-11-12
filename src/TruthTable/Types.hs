module TruthTable.Types where

import qualified Data.Map.Strict as Map 
import Data.List (nub)

type TruthSet = Map.Map Variable Bool

data TruthTable = TruthTable 
                { variables :: [Variable],
                  truthSets :: [TruthSet] }

data Operator = And
              | Or
              | Xor
              deriving (Eq, Ord, Show, Read, Bounded, Enum)

newtype Variable = Variable String
                   deriving (Eq, Ord, Show)

data Statement = StatementResult Bool
               | NestedStatement Statement
               | NegationStatement Statement
               | VariableStatement Variable
               -- ^ a statement that just wraps a variable so we can negate
               -- variables without further work
               | Statement Statement Operator Statement
                deriving (Eq, Show)
