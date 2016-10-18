QuipperDemo
============

This is a demonstration of several ways [Quipper](http://www.mathstat.dal.ca/~selinger/quipper/) can be used in designing circuits for quantum computing. More importantly, it's an excuse to play with [Haskell](https://www.haskell.org/) and quantum information processing at the same time! I created a [writeup](writeup/writeup.pdf) which contains more details than this README. 

# Key Features Demonstrated

- Generate circuit diagrams directly from the Haskell source!
- Perform generic circuit transformations - used here to filter out gates with small contributions.
- Deterministic simulation of the quantum circuits.


# Source Overview

- [Main.hs](src/Main.hs)
  - `qft`: A direct implementation of the [quantum fourier transform (QFT)](https://en.wikipedia.org/wiki/Quantum_Fourier_transform) using rotation, hadamard, and cnot gates.
  - `make_approx_qft`: Uses Quipper's generic [circuit transformation facilities](http://www.mathstat.dal.ca/~selinger/quipper/doc/Quipper-Transformer.html) to generate a circuit that approximates a QFT (but uses less gates).
  - `compare_circuits`: Use Quipper's ability to [simulate quatum circuits](http://www.mathstat.dal.ca/~selinger/quipper/doc/QuipperLib-Simulation-QuantumSimulation.html) to generically compute the difference between the outputs of two circuits. This is then used to compute the error in the approximation to the QFT.

# Notes

NB: Since building this, Quipper has added a QFT to its standard library (see [here](http://www.mathstat.dal.ca/~selinger/quipper/doc/QuipperLib-QFT.html)).
Also, a paper using Quipper for 5 standard quantum algorithms has been published (see [here](https://arxiv.org/pdf/1406.4481v2.pdf)).

This was all produced in a weekend sprint before graduating university (and interrupted by [BayHac 2014](https://wiki.haskell.org/BayHac2014)). If there's anything confusing / incorrect / poorly documented, please reach out! The functionality for generically comparing the performance of two circuits was done especially hastily.

Lastly, the existence / use of the function `CatchStdOut.hs::catchOutput` might confuse some people. This is a hack around a portion of the Quipper API which prints the circuit as an EPS file to stdout. In order to capture this output and convert it to png (within Haskell), this hack is required.
