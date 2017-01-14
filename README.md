# QuantumHaskell

This is a re-implementation and modification of Jan Skibinsk's module
QuantumVector, obtained from https://wiki.haskell.org/Numeric_Quest
A copy of the original code is included.

Initially the code is simply a copy of Jan SKibinski's but with minor cosmetic changes.
The plan is to further modify the code using more abstract mathematics, and perhaps
even including simple analytical methods.

The basic representation of a quantum mechanical state is the Bra state vector, kets are implemented as linear operators from the Bra space onto the complex numbers (called Scalars).

Operators are implemented using the closure relation in order to expand vectors into linear combinations of eigenvectors of the operator. The wrapper apply_op can be used in order to apply operators without explicitly calling the closure function. This does not generate pretty code, but using the code should be less cluttered.
