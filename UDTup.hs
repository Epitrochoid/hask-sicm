module UDTup 
    ( UDTup(..)
    ) where

import Data.Either

data UDTup a = Val a
             | Up (UDTup a)
             | Down (UDTup a)
             | Up2 (UDTup a) (UDTup a)
             | Down2 (UDTup a) (UDTup a)

