module TruthTable.Types where

import TruthTable.OneOf
import qualified Data.Map.Strict as Map 


data Operator = And
              | Or
              | Xor
              deriving (Eq, Ord, Show, Read, Bounded, Enum)

newtype Variable = Variable String
                   deriving (Eq, Ord, Show)

data Statement = NestedStatement Statement
               | NegationStatement Statement
               | VariableStatement Variable
               -- ^ a statement that just wraps a variable so we can negate
               -- variables without further work
               | Statement Statement Operator Statement
                deriving (Eq, Show)

