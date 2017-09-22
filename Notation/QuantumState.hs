module Notation.QuantumState
(QuantumState(..)
, closure
, Scalar
, showScalar
) where
import           Data.Complex

--------------------------------------------------------------------------------
-- Scalars are complex vauled numbers                                         --
--------------------------------------------------------------------------------
type Scalar = Complex Double

--------------------------------------------------------------------------------
-- The generic representation of a quantum mechanical state                   --
-- with basic functions defined for a quantum mechanical state                --
--------------------------------------------------------------------------------
class QuantumState a where
    add         :: a -> a -> a
    scale       :: Scalar -> a -> a
    reduce      :: a -> a
    basis       :: a -> [a]
    components  :: a -> [Scalar]
    compose     :: [Scalar] -> [a] -> a
    dim         :: a -> Int
    norm        :: a -> Double
    normalize   :: a -> a
    bracket     :: a -> a -> Scalar

    dim x   = length (basis x)

    normalize x
        | locnorm == 0  = x
        | otherwise     = compose coeffs (basis x)
         where
            coeffs      = [a/locnorm :+ b/locnorm| a :+ b <- components x]
            locnorm     = norm x

--------------------------------------------------------------------------------
-- Make Scalar an instance of Show in order to properly print scalar values   --
-- Scalars in quantum mechanics are complex numbers                           --
--------------------------------------------------------------------------------
showScalar :: (Show t, RealFloat t) => Int -> Complex t -> String -> String
showScalar n x@(a :+ b)
    | b == 0    = showsPrec n a
    | otherwise = showString "(" . showsPrec n x . showString ")"

--------------------------------------------------------------------------------
-- Outer product or closure relation                                          --
-- Used to apply an operator to a Ket vector by first expanding the Ket into  --
-- an eigenbasis of the operator and then applying the operator to each of    --
-- the eigenketss and finally recombining into a new Ket vector               --
-- |y> = A |x> = Sum k (A |k><k|x>) = Sum k,l (<k| cl*|l> A|k>) =             --
-- = Sum k,l (Ak * cl * d(k,l) * |k>) = Sum k (Ak * ck * |k>)                 --
--------------------------------------------------------------------------------
closure :: (QuantumState a, QuantumState b) => (a -> b) -> a -> b
closure operator x =
    compose' (components x) (map operator (basis x))
     where
        compose' v coeffs = foldl1 add (zipWith scale v coeffs)
