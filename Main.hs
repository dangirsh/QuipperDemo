import Quipper
import Quipper.QData (qubit)
import Common
import Control.Monad


qft :: [Qubit] -> Circ [Qubit]
qft [] = return []
qft [x] = do
    hadamard x
    return [x]
qft (x:xs) = do
    xs' <- qft xs
    xs'' <- rotations x xs' (length xs')
    x' <- hadamard x
    return (x':xs'')
    where
        rotations :: Qubit -> [Qubit] -> Int -> Circ [Qubit]
        rotations _ [] _ = return []
        rotations c (q:qs) n = do
            qs' <- rotations c qs n
            let m = ((n + 1) - length qs)
            q' <- rGate m q `controlled` c
            return (q':qs')


main = print_generic EPS qft (replicate 8 qubit)