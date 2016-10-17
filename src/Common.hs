module Common where

import Quipper (Circ, Qubit, print_generic, Format(EPS))
import Quipper.QData (qubit)
import Quantum.Synthesis.Ring (Cplx(Cplx), norm)
import System.Random (RandomGen, randomR)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import CatchStdOut (catchOutput)


type QFunc = [Qubit] -> Circ [Qubit]


output qf shape fname = do
    out <- catchOutput (print_generic EPS qf shape)
    writeFile (fname ++ ".eps") out


average :: [Double] -> Double
average ds = sum ds / (fromIntegral (length ds))


mag :: Cplx Double -> Double
mag (Cplx r i) = sqrt (r**2 + i**2)


normalize :: [Cplx Double] -> [Cplx Double]
normalize as = [Cplx (ar * s) (ai * s) | (Cplx ar ai) <- as]
    where
        n = (fromIntegral . length $ as)
        s = (1/2) ** (n/2)


meas :: (RandomGen g) => g -> [Cplx Double] -> Int
meas g cs = chosen
    where
        amps = map ((**2) . mag) cs
        (r, _) = randomR (0, 1) g
        chosen = fst . last . takeWhile (\(_, v) -> v<r) . zip [0..] . scanl (+) 0 $ amps