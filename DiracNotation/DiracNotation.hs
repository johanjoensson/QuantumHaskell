module DiracNotation
((*|)
,(>|)
, (|.|)
, (><)
, Scalar
, Ket(..)
, Bra(toKet)
, toBra
, applyOp
) where

import           Data.Complex
import           Data.List    (nub)
import           QuantumState

--------------------------------------------------------------------------------
-- Infix operators for nice and simple notation                               --
--------------------------------------------------------------------------------
infixl 5 +| -- Addition of Kets
infixl 5 +< -- Addition of Bras

infix 6 *|  -- Scalar multiplication with Ket
infix 6 *<  -- Scalar multiplication with Bra

infixl 7 >| -- Ket tensor product
-- infixl 7 |< -- Bra tensor product, not sure how to properly implement

infix 4 |.| -- Inner product
infix 5 ><  -- Outer product

--------------------------------------------------------------------------------
-- Data type for multidimensional Ket spaces                                  --
--------------------------------------------------------------------------------
data Tuple a b = a :* b
    deriving (Eq)

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
instance Eq a => QuantumState (Ket a) where
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
-- type Bra a = Ket a -> Scalar

--------------------------------------------------------------------------------
-- The Bra 'constructor' using bracket b a  = <b|a> = Scalar                  --
-- partial application gives bracket b = <b| = Ket a -> Scalar                --
-- which we use as our representation of the Bra vector                       --
--------------------------------------------------------------------------------
-- bra :: Eq a => Ket a -> Bra a
-- bra = bracket

--------------------------------------------------------------------------------
-- Ket tensor product                                                         --
--------------------------------------------------------------------------------
(>|) :: (Eq a, Eq b) => Ket a -> Ket b -> Ket (Tuple a b)
Ket a >| Ket b  = Ket (a :* b)
_ >| KetZero    = KetZero
KetZero >| _    = KetZero
x >| y          = foldl1 (:+|) [((toBra (Ket a) |.| x) * (toBra (Ket b) |.| y))
                  :*| Ket (a :* b) | Ket a <- basis x, Ket b <- basis y]
--------------------------------------------------------------------------------
-- Bra tensor product                                                         --
--------------------------------------------------------------------------------
(|<) :: (Eq a, Eq b) => Bra a -> Bra b -> Bra (Tuple a b)
a |< b  = toBra ( toKet a >| toKet b)

--------------------------------------------------------------------------------
-- Addition of two Ket vectors results in a Ket vector                        --
--------------------------------------------------------------------------------
(+|) :: Eq a => Ket a -> Ket a -> Ket a
x +| KetZero    = x
KetZero +| x    = x
x +| y          = reduce (x :+| y)

--------------------------------------------------------------------------------
-- Adding two Bra vectors results in a new linear operator Ket -> Scalar      --
-- (<a| + <b|)|c> = <a|c> + <b|c>                                             --
--------------------------------------------------------------------------------
(+<) :: Eq a => Bra a -> Bra a -> Bra a
x +< y          = toBra (reduce (toKet x +| toKet y))

--------------------------------------------------------------------------------
-- Multiplication of a Ket by a scalar results in a Ket vector                --
--------------------------------------------------------------------------------
(*|) :: Eq a => Scalar -> Ket a -> Ket a
s *| (x :+| y)      = (s *| x) +| (s *| y)
_ *| KetZero        = KetZero
0 *| _              = KetZero
s *| (t :*| x)      = (s*t) *| x
s *| x              = s :*| x

--------------------------------------------------------------------------------
-- Bra myltiplied by a scalar results in a linear operator Ket -> Scalar      --
-- (a * <b|)|c> = a * <b|c>                                                   --
--------------------------------------------------------------------------------
(*<) :: Eq a => Scalar -> Bra a -> Bra a
s *< x  = toBra (s *| toKet x)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- This part is shaky, at best!                                               !!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------------
-- Make Kets instances of Num, in order to simplify notation a bit            --
-- at least (+) and (-) are trivial to implement                              --
-- the other functions are less obvious, e.g. fromInteger seems impossible    --
--------------------------------------------------------------------------------
instance Eq a => Num (Ket a) where
    x + y   = x +| y
    x - y   = x +| ((-1) *| y)

--------------------------------------------------------------------------------
-- Two Kets are equal iff all their components are equal                      --
--------------------------------------------------------------------------------
instance Eq a => Eq (Ket a) where
    x == y   = and [coeff v x == coeff v y | v <- basis x]
      where
         coeff v z = toBra v |.| z

--------------------------------------------------------------------------------
-- Reduce a Ket to a sum of orthogonal basis Kets                             --
--------------------------------------------------------------------------------
reduceKet :: Eq a => Ket a -> Ket a
reduceKet x
         = compose coeffs v
          where
             v = basis x
             coeffs = [toBra vi |.| x | vi <- v]

--------------------------------------------------------------------------------
-- Extract the basis vectors from a Ket                                       --
--------------------------------------------------------------------------------
ketBasis :: Eq a => Ket a -> [Ket a]
ketBasis KetZero     = [KetZero]
ketBasis (Ket k)     = [Ket k]
ketBasis (_ :*| x)   = [x]
ketBasis (k1 :+| k2) = nub (ketBasis k1 ++ ketBasis k2)

--------------------------------------------------------------------------------
-- Converting a Ket into a Bra is simply applying bracket to the Ket          --
-- bracket a = <a| = (Ket -> Scalar)
-- toBra :: Eq a => Ket a -> Bra a
-- toBra   = bra

--------------------------------------------------------------------------------
-- The inner product between two QuantumStates, a and b, is defined as        --
-- <a|b> = (<a|)|b>, i.e. bracket a b = (bracket a) b                         --
--------------------------------------------------------------------------------
(|.|) :: (Eq a) => Bra a -> Ket a -> Scalar
-- b |.| k = b k
b |.| k = bracketBra b k

--------------------------------------------------------------------------------
-- In a Bra - Ket representation define the bracket in terms of Ket vectors   --
-- bracket |a> |b> = <a|b>, thus defining the dual Bra vectors as well        --
--------------------------------------------------------------------------------
bracketKet :: (Eq a) => Ket a -> Ket a -> Scalar
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
-- Monoids, because monoids. MONOIDS!!!                                       --
--------------------------------------------------------------------------------
instance Eq a => Monoid (Ket a) where
    mempty = KetZero
    k `mappend` l = k +| l

-------------------------------------------------------------------------------
-- Make Ket an instance of Functor. This might be useful for changing basis. --
-- With emphasis on the might part.                                          --
-------------------------------------------------------------------------------
instance Functor Ket where
    fmap f KetZero   = KetZero
    fmap f (Ket i)   = Ket (f i)
    fmap f (k :+| l) = fmap f k :+| fmap f l
    fmap f (s :*| k) = s :*| fmap f k

--------------------------------------------------------------------------------
-- Make Ket an instance of Applicative. Again this might be useful for        --
-- changing basis, but with an heavier emphasis on the might part than for    --
-- Functor.                                                                   --
--------------------------------------------------------------------------------
instance Applicative Ket where
    pure = Ket
    KetZero <*> _ = KetZero
    Ket f <*> k = fmap f k

--------------------------------------------------------------------------------
-- Make Ket a an instance of Show, in order to print Ket vectors in a pretty  --
-- way. Since Bra vectors are functions in Haskell they cannot be made an     --
-- instance of Show and thus cannot be printed                                --
--------------------------------------------------------------------------------
instance (Show a, Eq a) => Show (Ket a) where
    showsPrec _ KetZero   = showString "Zero-Ket"
    showsPrec n (Ket j)   = showString "|" . showsPrec n j . showString ">"
    showsPrec n (x :*| k) = showScalar n x . showsPrec n k
    showsPrec n (j :+| k) = showsPrec n j . showSign n k . showsPrec n k

--------------------------------------------------------------------------------
-- Function to improve the prettyness of the printing.                        --
-- This function fixes the printing of negative coefficients.                 --
--------------------------------------------------------------------------------
showSign :: (Show a, Eq a) => Int -> Ket a -> String -> String
showSign n (Ket j) = showString " + "
showSign n (s@(a :+ b) :*| k)
    | b == 0, a < 0 = showString ""
    | otherwise = showString " + "

--------------------------------------------------------------------------------
-- Make Tuple (a b) an instance of Show in order to properly print Kets of    --
-- all possible basis Kets                                                    --
--------------------------------------------------------------------------------
instance (Show a, Show b) => Show (Tuple a b) where
    showsPrec n (a :* b) = showsPrec n a . showString "; " . showsPrec n b

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- HIGHLY EXPERIMENTAL STUFF STARTS here
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------------
-- Alternative implementation of Bra vectors, using more advanced stuff from  --
-- functional programming.                                                    --
--------------------------------------------------------------------------------
data Bra a = Bra { bracketBra :: Ket a -> Scalar, toKet :: Ket a }

--------------------------------------------------------------------------------
-- Convert Ket vectors into Bra vectors                                       --
--------------------------------------------------------------------------------
toBra :: (Eq a) => Ket a -> Bra a
toBra k = Bra (bracket k) k

--------------------------------------------------------------------------------
-- As for Ket vectors, this part is shaky but it simplifies notation.         --
--------------------------------------------------------------------------------
instance (Eq a) => Num (Bra a) where
    (Bra f a) + (Bra g b) = Bra f' a'
        where a' = a + b
              f' = bracket a'
    (Bra f a) - (Bra g b) = Bra f' a'
        where a' = a - b
              f' = bracket a'

--------------------------------------------------------------------------------
-- Monoids! Because MONOIDS!!!                                                --
--------------------------------------------------------------------------------
instance (Eq a) => Monoid (Bra a) where
    mempty = toBra KetZero
    (Bra k a) `mappend` (Bra l b) = Bra (bracket a') a'
        where a' = a + b

--------------------------------------------------------------------------------
-- Printing. So pretty!                                                       --
--------------------------------------------------------------------------------
instance (Show a, Eq a) => Show (Bra a) where
    showsPrec _ (Bra _ KetZero)   = showString "Zero-Bra"
    showsPrec n (Bra _ (Ket j))   = showString "<" . showsPrec n j .
        showString "|"
    showsPrec n (Bra f (x :*| k)) = showScalar n x' .
        showsPrec n (Bra f k)
            where x' = conjugate x
    showsPrec n (Bra f (j :+| k)) = showsPrec n (Bra f j) .
        showSign n k . showsPrec n (Bra f k)
