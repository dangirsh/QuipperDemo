{-# LANGUAGE  RankNTypes #-}

import Quipper
import Quipper.Monad (controlled)
import Quipper.QData (BType)
import QuipperLib.Simulation (sim_amps)
import Quantum.Synthesis.Ring (Cplx(Cplx))
import Control.Monad (unless, replicateM)
import System.Random (mkStdGen, randoms)
import Data.Map (Map, fromList, toAscList)
import Common


-- Implementation of the quantum Fourier transform (https://en.wikipedia.org/wiki/Quantum_Fourier_transform)
qft :: QFunc
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
            let m = (n + 1) - length qs
            q' <- rGate m q `controlled` c
            return (q':qs')


-- Removes all rotations smaller than 2 pi i / theta, where theta = 2 ^ (l + 1)
-- where l the maximum distance between a rotation and the corresponding cnot
make_approx_transformer :: Int -> Transformer Circ Qubit Bit
make_approx_transformer l = transformer
    where
        transformer :: Transformer Circ Qubit Bit
        transformer (T_QRot name 1 0 inv theta ncf f) = f $
            \[q0] [] ctrls ->
                without_controls_if ncf $
                with_controls ctrls $ do
                    unless (l < l') $ rGate_at l' q0
                    return ([q0], [], ctrls)
            where
                l' = (round $ logBase 2 theta) - 1
        transformer g = identity_transformer g



make_approx_qft :: Int -> QFunc
make_approx_qft l = transform_generic (make_approx_transformer l) qft


approx_qft = make_approx_qft 8


-- Number of qubits in the circuit
n = 8


output_circuit_diagrams :: IO ()
output_circuit_diagrams = do
    output qft input_shape "qft"
    output approx_qft input_shape "approx_qft"
    where
        input_shape = replicate n qubit


-- simulate each circuit num_sims times and return the error rate
compare_circuits :: QFunc -> QFunc -> Int -> Double
compare_circuits c1 c2 num_sims = error_rate
    where
        error_rate = fromIntegral (length (filter id results)) / fromIntegral num_sims
        results = [meas g (sim c1 g) == meas g (sim c2 g) | s <- [1..num_sims], let g = mkStdGen s]
        sim circuit g = map snd . toAscList $ sim_amps g circuit $ input_state g
        input_state = make_state . normalize . take n . make_amplititudes . randoms
        make_state = fromList . zip basis_states
        basis_states = replicateM n [True, False]
        make_amplititudes (r1:r2:rs) = (Cplx r1 r2 :: Cplx Double) : make_amplititudes rs


main :: IO ()
main = do
    output_circuit_diagrams
    print $ compare_circuits qft approx_qft 3
