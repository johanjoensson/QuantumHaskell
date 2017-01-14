# QuantumHaskell

This is a re-implementation and modification of Jan Skibinsk's module
QuantumVector, obtained from https://wiki.haskell.org/Numeric_Quest
A copy of the original code is included.

This code implements Dirac's notation of quantum mechanics in Haskell.

Initially the code is simply a copy of Jan SKibinski's but with minor cosmetic changes.
The plan is to further modify the code using more abstract mathematics, and perhaps
even including simple analytical methods.

The basic goal of this module is to allow a notation that will look similar to how quantum mechanics is written by hand. Another goal is to make the module easy to use without having to worry too much about Haskell specific limitations and complications, while still using abstract mathematical concepts in order to represent different properties.

This module also includes an implementation of the abstract algebra representation of quantum mechanics, originally developed by Dirac.
The basic representation of a quantum mechanical state is the Ket state vector, the dual vectors, Bra vectors, are implemented as linear operators from the Ket space onto the complex numbers (called Scalars).

Operators are implemented using the closure relation in order to expand vectors into linear combinations of eigenvectors of the operator. The wrapper apply_op can be used in order to apply operators without explicitly calling the closure function. This does not generate pretty code in the module, but using the module should lead to less cluttered notation.
