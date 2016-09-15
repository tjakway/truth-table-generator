module Main where

import Data.Map.Strict (lookup)

-- | lookup or die with an informative error
-- basically fromJust with a bad excuse for diagnostics
lookupOrFail item mapping = case lookup item mapping of 
                                      Just x  -> x
                                      Nothing -> error ("Lookup of " ++ (show item) ++ " in " ++ (show mapping) ++ " failed!")

--Unbiased Either type
--couldn't find an implementation of this within 5 minutes of googling
data OneOf a b = Up a | Down b

--analogous to the function either
forOneOf :: (a -> c) -> (b -> d) -> OneOf a b -> OneOf c d
forOneOf f g (Up a)   = Up   (f a)
forOneOf f g (Down b) = Down (g b)

data Operator = And
              | Or
              | Xor

newtype Variable = Variable String

data Statement = Negation Statement
               | VariableStatement Variable
               -- ^ a statement that just wraps a variable so we can negate
               -- variables without further work
               | Statement (OneOf Statement Variable) Operator (OneOf Statement Variable)

evaluateStatement :: Map Variable Bool -> Statement -> Bool
evaluateStatement vars (Negation stmt) = not . evaluateStatement vars $ stmt
evaluateStatement vars (VariableStatement thisVar) = 
evaluateStatement vars (Statement (Up stmt) op _) = evaluateStatement vars stmt
evaluateStatement vars (Statement _ op (Up stmt)) = evaluateStatement vars stmt
evaluateStatement vars (Statement (Down first) op (Down second)) = evaluateOperator op first second

evaluateOperator :: Operator -> Bool -> Bool -> Bool
evaluateOperator op first second 
                    | op == And = first && second
                    | op == Or  = first || second
                    | op == Xor = (first || second) && (first /= second)


main :: IO ()
main = putStrLn "Hello, Haskell!"
