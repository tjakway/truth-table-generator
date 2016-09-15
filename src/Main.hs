module Main where

import Data.Map

--Unbiased Either type
data OneOf a b = Up a | Down b

oneOf :: (a -> c) -> (b -> d) -> OneOf a b -> OneOf c d
oneOf f g (Up a)   = Up   (f a)
oneOf f g (Down b) = Down (g b)


data Operator = And
              | Or
              | Xor

newtype Variable = Variable String

data Statement = Negation Statement
               | Statement (OneOf Statement Variable) Operator (OneOf Statement Variable)

evaluate :: Map Variable Bool -> Statement -> Bool
evaluate vars (Negation stmt) = not . evaluate vars $ stmt

main :: IO ()
main = putStrLn "Hello, Haskell!"
