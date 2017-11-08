# QuantumHaskell

This is a re-implementation and modification of Jan Skibinsk's module
QuantumVector, obtained from https://wiki.haskell.org/Numeric_Quest A copy of
the original code is included.

This code implements Dirac's notation of quantum mechanics in Haskell.

Initially the code is simply a copy of Jan SKibinski's but with minor cosmetic
changes.  The plan is to further modify the code using more abstract
mathematics, and perhaps even including simple analytical methods.

The basic goal of this module is to allow a notation that will look similar to
how quantum mechanics is written by hand. Another goal is to make the module
easy to use without having to worry too much about Haskell specific limitations
and complications, while still using abstract mathematical concepts in order to
represent different properties.

This module also includes an implementation of the abstract algebra
representation of quantum mechanics, originally developed by Dirac.  The basic
representation of a quantum mechanical state is the Ket state vector, the dual
vectors, Bra vectors, are implemented as linear operators from the Ket space
onto the complex numbers (called Scalars).

Operators are implemented using the closure relation in order to expand vectors
into linear combinations of eigenvectors of the operator. The wrapper apply_op
can be used in order to apply operators without explicitly calling the closure
function. This does not generate pretty code in the module, but using the
module should lead to less cluttered notation.

Also included is a package for automatic differentiation, based on the work by
Jerzy Karczmarczuk in "Functional Differentiaion of Computer Programs",
Higher-Order and Symbolic Computation, 14, 35-57, 2001. Kluwer Academic
Publishers. This package is used in order to calculate the wave-functions for
the energy eigenstates for the harmonic oscillator, using Hermite polynomials.
As suggested by Jerzy Karczmarczuk, in Scientific Computation and Functional
Programming.

[![Build Status](https://travis-ci.org/johanjoensson/QuantumHaskell.svg?branch=master)](https://travis-ci.org/johanjoensson/QuantumHaskell)
[![Coverage Status](https://coveralls.io/repos/github/johanjoensson/QuantumHaskell/badge.svg?branch=master)](https://coveralls.io/github/johanjoensson/QuantumHaskell?branch=master)
