import SymCalc
import qualified Data.Vector as V
import Data.Either

data UDTup a = Up Vector a
             | Down Vector a

instance (Num a) => Either Num (UDTup a) where
        (+) (Up a) (Down a) = Left "Cannot add an up and down tuple"
        (+) (Down a) (Up a) = Left "Cannot add an up and down tuple"
        (+) (Up a) (Up b) = Right $ Up $ V.zipWith (+) a b
        (+) (Down a) (Down b) = Right $ Down $ V.zipWith (+) a b
        (-) (Up a) (Down a) = Left "Cannot subtract an up and down tuple"
        (-) (Down a) (Up a) = Left "Cannot subtract an up and down tuple"
        (-) (Up a) (Up b) = Right $ Up $ V.zipWith (-) a b
        (-) (Down a) (Down b) = Right $ Down $ V.zipWith (-) a b
