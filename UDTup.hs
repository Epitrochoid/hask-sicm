module UDTup 
    ( UDTup(..)
    ) where

import Data.Either

data UDTup a = Val a
             | Up [UDTup a]
             | Down [UDTup a]
             | Undef String
             deriving (Show)

instance (Num a) => Num (UDTup a) where
        -- Addition
        (Val a) + (Val b) = Val $ a + b
        (Up a) + (Up b) = Up $ zipWith (+) a b
        (Down a) + (Down b) = Down $ zipWith (+) a b
        (Up _) + (Down _) = Undef "Cannot add Up and Down tuple"
        (Down _) + (Up _) = Undef "Cannot add Down and Up tuple"
        (Up _) + (Val _) = Undef "Cannot add Up tuple to value"
        (Val _) + (Up _) = Undef "Cannot add value to Up tuple"
        (Down _) + (Val _) = Undef "Cannot add Down tuple to value"
        (Val _) + (Down _) = Undef "Cannot add value to Down tuple"
        (Undef a) + _ = a
        _ + (Undef a) = a
        -- Subtraction
        (Val a) - (Val b) = Val $ a - b

