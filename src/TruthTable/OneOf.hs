module TruthTable.OneOf where

--Unbiased Either type
--couldn't find an implementation of this within 5 minutes of googling
data OneOf a b = Up a | Down b
    deriving (Eq, Show)

--analogous to the function either
forOneOf :: (a -> c) -> (b -> d) -> OneOf a b -> OneOf c d
forOneOf f g (Up a)   = Up   (f a)
forOneOf f g (Down b) = Down (g b)
