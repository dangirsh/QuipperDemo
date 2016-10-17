QuipperDemo
============

This is a demonstration of several ways [Quipper](http://www.mathstat.dal.ca/~selinger/quipper/) can be used in designing circuits for quantum computing,

# Overview:

    - [Main.hs](src/Main.hs)
      - `qft`: A direct implementation of the [quantum fourier transform (QFT)](https://en.wikipedia.org/wiki/Quantum_Fourier_transform) using rotation, hadamard, and cnot gates.
      - `make_approx_qft`: Uses Quipper's generic [circuit transformation facilities](http://www.mathstat.dal.ca/~selinger/quipper/doc/Quipper-Transformer.html) to generate an circuit that approximates a QFT (but uses less gates).
      - `compare_circuits`: Use Quipper's ability to [simulate quatum circuits](http://www.mathstat.dal.ca/~selinger/quipper/doc/QuipperLib-Simulation-QuantumSimulation.html) to generically compute the error


NB: Since building this, Quipper has added a QFT to its standard library (see [here](http://www.mathstat.dal.ca/~selinger/quipper/doc/QuipperLib-QFT.html)). Also, a paper using Quipper for QFTs has been published (see [here](https://arxiv.org/pdf/1406.4481v2.pdf)).

This was all produced in a weekend sprint before graduating (and interrupted by [BayHac 2014](https://wiki.haskell.org/BayHac2014)). If there's anything confusing / incorrect / poorly documented, please reach out! The functionality for generically comparing the performance of two circuits was done especially hastily.