module UDTup 
    ( UDTup(..)
    ) where

import Data.Either

data UDTup a = Val a
             | Up (UDTup a)
             | Down (UDTup a)
             | Up2 (UDTup a) (UDTup a)
             | Down2 (UDTup a) (UDTup a)
             | Undef String
             deriving (Show)

instance (Num a) => Num (UDTup a) where
        (Val a) + (Val b) = Val $ a + b
        (Up a) + (Up b) = Up $ a + b
        (Up2 a b) + (Up2 c d) = Up2  (a+c) (b+d)
        (Down a) + (Down b) = Down $ a + b
        (Down2 a b) + (Down2 c d) = Down2  (a+c) (b+d)
        (Val _) + _ = Undef "Sum error"
        (Up _) + _ = Undef "Sum error"
        (Up2 _ _) + _ = Undef "Sum error"
        (Down _) + _ = Undef "Sum error"
        (Down2 _ _) + _ = Undef "Sum error"
        (Val a) - (Val b) = Val $ a - b
        (Up a) - (Up b) = Up $ a - b
        (Up2 a b) - (Up2 c d) = Up2  (a-c) (b-d)
        (Down a) - (Down b) = Down $ a - b
        (Down2 a b) - (Down2 c d) = Down2  (a-c) (b-d)
        (Val _) - _ = Undef "Difference error"
        negate = undefined
        abs = undefined
        fromInteger a = Val (fromInteger a)



