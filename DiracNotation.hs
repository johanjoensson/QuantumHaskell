module DiracNotation where
import           Data.Complex
import           Data.List    (nub)

--------------------------------------------------------------------------------
-- Infix operators for nice and simple notation                               --
--------------------------------------------------------------------------------
infixl 5 +| -- Addition of Kets
infixl 5 +< -- Addition of Bras

infix 6 *|  -- Scalar multiplication with Ket
infix 6 *<  -- Scalar multiplication with Bra

infixl 7 >| -- Ket tensor product
-- infixl 7 |<     -- Bra tensor product, not sure how to properly implement

infix 4 |.| -- Inner product
infix 5 ><  -- Outer product

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
-- Data type for multidimensional Ket spaces                                  --
--------------------------------------------------------------------------------
data Tuple a b = a :* b
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Ket constructors                                                           --
--------------------------------------------------------------------------------
data Ket a=
    KetZero         -- 0 vector
    | Ket a
    | Scalar :*| Ket a
    | Ket a :+| Ket a

--------------------------------------------------------------------------------
-- Make our Ket vectors instances of the QuantumState type class              --
-- and define appropriate functions on Kets                                   --
--------------------------------------------------------------------------------
instance Ord a => QuantumState (Ket a) where
    add             = (+|)
    scale           = (*|)
    reduce          = reduceKet
    basis           = ketBasis
    components x    = [toBra e |.| x | e <- basis x]
    compose coeffs v= foldl1 (:+|) [uncurry (:*|) z | z <- zip coeffs v]
    norm KetZero = 0
    norm x       = sqrt $ realPart (toBra x |.| x)
    bracket         = bracketKet

--------------------------------------------------------------------------------
-- Define Bra vectors as linear functions from Ket vectors into Scalars       --
--------------------------------------------------------------------------------
type Bra a = Ket a -> Scalar
--------------------------------------------------------------------------------
-- The Bra 'constructor' using bracket b a  = <b|a> = Scalar                  --
-- partial application gives bracket b = <b| = Ket a -> Scalar                --
-- which we use as our representation of the Bra vector                       --
--------------------------------------------------------------------------------
bra :: Ord a => Ket a -> Bra a
bra = bracket

--------------------------------------------------------------------------------
-- Ket tensor product                                                         --
--------------------------------------------------------------------------------
(>|) :: (Ord a, Ord b) => Ket a -> Ket b -> Ket (Tuple a b)
Ket a >| Ket b  = Ket (a :* b)
_ >| KetZero    = KetZero
KetZero >| _    = KetZero
x >| y          = foldl1 (:+|) [((bra (Ket a) |.| x) * (bra (Ket b) |.| y))
                  :*| Ket (a :* b) | Ket a <- basis x, Ket b <- basis y]

--------------------------------------------------------------------------------
-- Addition of two Ket vectors results in a Ket vector                        --
--------------------------------------------------------------------------------
(+|) :: Ord a => Ket a -> Ket a -> Ket a
x +| KetZero    = x
KetZero +| x    = x
x +| y          = reduce (x :+| y)

--------------------------------------------------------------------------------
-- Adding two Bra vectors results in a new linear operator Ket -> Scalar      --
-- (<a| + <b|)|c> = <a|c> + <b|c>                                             --
--------------------------------------------------------------------------------
(+<) :: Ord a => Bra a -> Bra a -> Bra a
x +< y          = sum . (`map` [x,y]) . flip ($)

--------------------------------------------------------------------------------
-- Multiplication of a Ket by a scalar results in a Ket vector                --
--------------------------------------------------------------------------------
(*|) :: Ord a => Scalar -> Ket a -> Ket a
s *| (x :+| y)      = (s *| x) +| (s *| y)
_ *| KetZero        = KetZero
0 *| _              = KetZero
s *| (t :*| x)      = (s*t) *| x
s *| x              = s :*| x

--------------------------------------------------------------------------------
-- Bra myltiplied by a scalar results in a linear operator Ket -> Scalar      --
-- (a * <b|)|c> = a * <b|c>                                                   --
--------------------------------------------------------------------------------
(*<) :: Ord a => Scalar -> Bra a -> Bra a
s *< x                   = (s*) . x

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- This part is shaky, at best!                                               !!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------------
-- Make Kets instances of Num, in order to simplify notation a bit            --
-- at least (+) and (-) are trivial to implement                              --
-- the other functions are less obvious, e.g. fromInteger seems impossible    --
--------------------------------------------------------------------------------
instance Ord a => Num (Ket a) where
    x + y   = x +| y
    x - y   = x +| ((-1) *| y)
--------------------------------------------------------------------------------
-- Two Kets are equal iff all their components are equal                      --
--------------------------------------------------------------------------------
instance (Eq a, Ord a) => Eq (Ket a) where
    x == y   = and [coeff v x == coeff v y | v <- basis x]
      where
         coeff v z = toBra v |.| z

--------------------------------------------------------------------------------
-- Reduce a Ket to a sum of orthogonal basis Kets                             --
--------------------------------------------------------------------------------
reduceKet :: Ord a => Ket a -> Ket a
reduceKet x
         = compose coeffs v
          where
             v = basis x
             coeffs = [toBra vi |.| x | vi <- v]

--------------------------------------------------------------------------------
-- Extract the basis vectors from a Ket                                       --
--------------------------------------------------------------------------------
ketBasis :: Ord a => Ket a -> [Ket a]
ketBasis KetZero     = [KetZero]
ketBasis (Ket k)     = [Ket k]
ketBasis (_ :*| x)   = [x]
ketBasis (k1 :+| k2) = nub (ketBasis k1 ++ ketBasis k2)

--------------------------------------------------------------------------------
-- Converting a Ket into a Bra is simply applying bracket to the Ket          --
-- bracket a = <a| = (Ket -> Scalar)
toBra :: Ord a => Ket a -> Bra a
toBra   = bra

--------------------------------------------------------------------------------
-- The inner product between two QuantumStates, a and b, is defined as        --
-- <a|b> = (<a|)|b>, i.e. bracket a b = (bracket a) b                         --
--------------------------------------------------------------------------------
(|.|) :: (Ord a) => Bra a -> Ket a -> Scalar
b |.| k = b k

--------------------------------------------------------------------------------
-- In a Bra - Ket representation define the bracket in terms of Ket vectors   --
-- bracket |a> |b> = <a|b>, thus defining the dual Bra vectors as well        --
--------------------------------------------------------------------------------
bracketKet :: (Ord a) => Ket a -> Ket a -> Scalar
bracketKet KetZero _       = 0
bracketKet _ KetZero       = 0
bracketKet (Ket i) (Ket j) = d i j -- Assuming the basis Kets are orthonormal
bracketKet (p :*| x) y     = conjugate p * bracketKet x y
bracketKet x (p :*| y)     = p * bracketKet x y
bracketKet (x1 :+| x2) y   = bracketKet x1 y + bracketKet x2 y
bracketKet x (y1 :+| y2)   = bracketKet x y1 + bracketKet x y2

--------------------------------------------------------------------------------
-- Kroenecker dela function, used as the inner product of two basis Kets      --
--------------------------------------------------------------------------------
d :: Eq a => a -> a -> Scalar
d i j
    | i == j    = 1
    | otherwise = 0

--------------------------------------------------------------------------------
-- Outer product or closure relation                                          --
-- Used to apply an operator to a Ket vector by first expanding the Ket into  --
-- an eigenbasis of the operator and then applying the operator to each of    --
-- the eigenketss and finally recombining into a new Ket vector             --
-- |y> = A |x> = Sum k (A |k><k|x>) = Sum k,l (<k| cl*|l> A|k>) =             --
-- = Sum k,l (Ak * cl * d(k,l) * |k>) = Sum k (Ak * ck * |k>)                 --
--------------------------------------------------------------------------------
closure :: (QuantumState a, QuantumState b) => (a -> b) -> a -> b
closure operator x =
    compose' (components x) (map operator (basis x))
     where
        compose' v coeffs = foldl1 add (zipWith scale v coeffs)

--------------------------------------------------------------------------------
-- nicer notation for applying the closure expansion                          --
--------------------------------------------------------------------------------
(><) :: (QuantumState b, QuantumState a) => (a -> b) -> a -> b
operator >< x   = closure operator x

--------------------------------------------------------------------------------
-- Helper function to avoid having to use the closure relation                --
-- when applying operators to Ket vectors                                     --
--------------------------------------------------------------------------------
applyOp :: (QuantumState a, QuantumState b) => (a -> b) -> a -> b
applyOp f x = f >< x

--------------------------------------------------------------------------------
-- Make Ket a an instance of Show, in order to print Ket vectors in a pretty  --
-- way. Since Bra vectors are functions in Haskell they cannot be made an     --
-- instance of Show and thus cannot be printed                                --
--------------------------------------------------------------------------------
instance (Show a, Eq a, Ord a) => Show (Ket a) where
    showsPrec _ KetZero   = showString "Zero-Ket"
    showsPrec n (Ket j)   = showString "|" . showsPrec n j . showString ">"
    showsPrec n (x :*| k) = showsScalar n x . showsPrec n k
    showsPrec n (j :+| k) = showsPrec n j . showString " + " . showsPrec n k

--------------------------------------------------------------------------------
-- Make Scalar an instance of Show in order to properly print scalar values   --
-- Scalars in quantum mechanics are complex numbers                           --
--------------------------------------------------------------------------------
showsScalar :: (Show t, RealFloat t) => Int -> Complex t -> String -> String
showsScalar n x@(a :+ b)
    | b == 0    = showsPrec n a . showString " "
    | otherwise = showString "(" . showsPrec n x . showString ") "
--------------------------------------------------------------------------------
-- Make Tuple (a b) an instance of Show in order to properly print Kets of    --
-- all possible basis Kets                                                    --
--------------------------------------------------------------------------------
instance (Show a, Show b) => Show (Tuple a b) where
    showsPrec n (a :* b) = showsPrec n a . showString "; " . showsPrec n b
