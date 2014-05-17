module Common where

import Quipper (Circ, Qubit, hadamard, controlled_not, label, print_generic, Format(EPS))
import Quipper.QData (qubit)
import CatchStdOut (catchOutput)


--output qf shape fname = do
--    out <- catchOutput (print_generic EPS qf shape)
--    writeFile (fname ++ ".eps") out
