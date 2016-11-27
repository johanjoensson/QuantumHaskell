module DiracNotation where
import Data.Complex
import Data.List (nub)




infixl 5 +|	-- Addition of Kets
infixl 5 +<	-- Addition of Bras

infix 6 *|	-- Scalar multiplication with Ket
infix 6 *<	-- Scalar multiplication with Bra

infixl 7 >|	-- Ket tensor product
infixl 7 |<	-- Bra tensor product

infix 4 |.|	-- Inner product
infix 5 ><	-- Outer product

type Scalar = Complex Double

class DiracVector a where
    add		:: a -> a -> a
    scale	:: Scalar -> a -> a
    reduce	:: a -> a
    basis	:: a -> [a]
    components	:: a -> [Scalar]
    compose	:: [Scalar] -> [a] -> a 
    dim		:: a -> Int
    norm	:: a -> Double
    normalize	:: a -> a

    dim x   = length (basis x)

    normalize x
	| locnorm == 0	= x
	| otherwise	= compose coeffs (basis x)
	 where
	    coeffs	= [a/locnorm :+ b/locnorm| a :+ b <- components x]
	    locnorm	= norm x

data Tuple a b = a :* b
    deriving (Eq, Ord)

data Ket a =
    KetZero	    -- 0 vector
    |Ket a
    |Scalar :*| Ket a
    |Ket a :+| Ket a
    |Ket a :-| Ket a

data Bra a=
    BraZero	    -- 0 vector
    | Bra a
    | Scalar :*< Bra a
    | Bra a :+< Bra a

instance Ord a => DiracVector (Ket a) where
    add		    = (+|)
    scale	    = (*|)
    reduce	    = reduceKet
    basis	    = ketBasis
    components x    = [toBra e |.| x | e <- basis x]
    compose coeffs v= foldl1 (:+|) [fst z :*| snd z | z <- zip coeffs v]
    norm KetZero    = 0
    norm x	    = sqrt $ realPart (toBra x |.| x)

instance Ord a => DiracVector (Bra a) where
    add		    = (+<)
    scale	    = (*<)
    reduce	    = reduceBra
    basis	    = braBasis
    components x    = [x |.| toKet e | e <- basis x]
    compose coeffs v= foldl1 (:+<) [fst z :*< snd z | z <- zip coeffs v]
    norm BraZero    = 0
    norm x	    = sqrt $ realPart (x |.| toKet x) 

-- Ket tensor product
(>|) :: (Ord a, Ord b) => Ket a -> Ket b -> Ket (Tuple a b)
Ket a	>|  Ket b   = Ket (a :* b)
_	>|  KetZero = KetZero
KetZero	>|  _	    = KetZero
x	>|  y	    = foldl1 (:+|) [((Bra a |.| x) * (Bra b |.| y)) :*| Ket (a :* b) | Ket a <- basis x, Ket b <- basis y]

-- Bra tensor product
(|<) :: (Ord a, Ord b) => Bra a -> Bra b -> Bra (Tuple a b)
Bra a |< Bra b	    = Bra (a :* b)
_ |< BraZero	    = BraZero
BraZero |< _	    = BraZero
x |< y		    = foldl1 (:+<) [((x |.| Ket a) * (y |.| Ket b)) :*< Bra (a :* b) | Bra a <- basis x, Bra b <- basis y]


-- Addition of Ket vectors
(+|) :: Ord a => Ket a -> Ket a -> Ket a
x +| KetZero	= x
KetZero +| x	= x
x +| y		= reduce (x :+| y)

-- Addition of Bra vectors
(+<) :: Ord a => Bra a -> Bra a -> Bra a
x +< BraZero	= x
BraZero +< x	= x
x +< y		= reduce (x :+< y)

-- Scalar multiplication of Kets
(*|) :: Ord a => Scalar -> Ket a -> Ket a
s *| (x :+| y)	    = (s *| x) +| (s *| y)
_ *| KetZero	    = KetZero
0 *| _		    = KetZero
s *| (t :*| x)	    = (s*t) *| x
s *| x		    = s :*| x

-- Scalar multiplication of Bras
(*<) :: Ord a => Scalar -> Bra a -> Bra a
s *< (x :+< y)	    = (s :*< x) +< (s :*< y)
_ *< BraZero	    = BraZero
0 *< _		    = BraZero
s *< (t :*< x)	    = (s*t) *< x
s *< x		    = s :*< x

-- This part is shaky, at best!
-- Add Kets and multiply Kets by scalars
instance Ord a => Num (Ket a) where
    x + y   = x +| y
    x - y   = x +| ((-1) *| y)

-- Add Bras and myltiply Bras by scalars
instance Ord a => Num (Bra a) where
    x + y   = x +< y
    x - y   = x +< ((-1) *< y)

-- This part should be fine

-- Compare Kets
instance (Eq a, Ord a) => Eq (Ket a) where
    x == y	= and [coeff v x == coeff v y | v <- basis x]
	 where
	    coeff v z = (toBra v) |.| z
-- Compare Bras
instance (Eq a, Ord a) => Eq (Bra a) where
    x == y	= and [coeff v x == coeff v y | v <- basis x]
	 where
	    coeff v z = z |.| toKet v

reduceKet :: Ord a => Ket a -> Ket a
reduceKet x
	    = compose coeffs v
	     where
		v = basis x
		coeffs = [toBra vi |.| x | vi <- v]
reduceBra :: Ord a => Bra a -> Bra a
reduceBra x
	    = compose coeffs v
	     where
		v = basis x
		coeffs = [x |.| toKet vi | vi <- v]


-- Extract the basis vectors of the Ket
ketBasis :: Ord a => Ket a -> [Ket a]
ketBasis KetZero    = []
ketBasis (Ket k)    = [Ket k]
ketBasis (_ :*| x)    = [x]
ketBasis (k1 :+| k2)  = nub (ketBasis k1 ++ ketBasis k2)

-- Extract the basis vectors of the Bra
braBasis :: Ord a => Bra a -> [Bra a]
braBasis BraZero    = []
braBasis (Bra b)    = [Bra b]
braBasis (_ :*< x)    = [x]
braBasis (b1 :+< b2)  = nub (braBasis b1 ++ braBasis b2)

-- Convert from ket to bra
toBra :: Ord a => Ket a -> Bra a
toBra (Ket k)	= Bra k
toBra (x :+| y)	= toBra x :+< toBra y
toBra (s :*| x)	= (conjugate s) :*< toBra x

-- Convert from Bra to Ket
toKet :: Ord a => Bra a -> Ket a
toKet (Bra b)	= Ket b
toKet (x :+< y)	= toKet x :+| toKet y
toKet (p :*< x)	= (conjugate p) :*| toKet x

-- Inner product
(|.|) :: Ord a => Bra a -> Ket a -> Scalar
BraZero	|.| _	    = 0
_ |.| KetZero	    = 0
Bra i |.| Ket j	    = d i j
(p :*< x) |.| (q :*| y) = p * q * (x |.| y)
(p :*< x) |.| y	    = p * (x |.| y)
x |.| (q :*| y)	    = q * (x |.| y)
(x1 :+< x2) |.| y	    = (x1 |.| y) + (x2 |.| y)
x |.| (y1 :+| y2)	    = (x |.| y1) + (x |.| y2)

-- Kroenecker dela function
d :: Eq a => a -> a -> Scalar
d i j
    | i == j	= 1
    | otherwise	= 0 

-- Outer product
closure :: (DiracVector a, DiracVector b) => (a -> b) -> a -> b
closure operator x =
    compose' (components x) (map operator (basis x))
     where
	compose' v coeffs = foldl1 add (zipWith scale v coeffs)

(><) :: (DiracVector b, DiracVector a) => (a -> b) -> a -> b
operator >< x	= closure operator x

-- Pretty printing stuff
instance (Show a, Eq a, Ord a) => Show (Ket a) where
    showsPrec _ KetZero	    = showString "Zero-Ket"
    showsPrec n (Ket j)	    = showString "|" . showsPrec n j . showString ">"
    showsPrec n (x :*| k)   = showsScalar n x . showsPrec n k
    showsPrec n (j :+| k)   = showsPrec n j . showString " + " . showsPrec n k

instance (Show a, Eq a, Ord a) => Show (Bra a) where
    showsPrec _ BraZero	    = showString "Zero-Bra"
    showsPrec n (Bra j)	    = showString "<" . showsPrec n j . showString "|"
    showsPrec n (x :*< k)   = showsScalar n x . showsPrec n k
    showsPrec n (j :+< k)   = showsPrec n j . showString " + " . showsPrec n k

showsScalar :: (Show t, RealFloat t) => Int -> Complex t -> String -> String
showsScalar n x@(a :+ b)
    | b == 0	= showsPrec n a . showString " "
    | otherwise	= showString "(" . showsPrec n x . showString ") "

instance (Show a, Show b) => Show (Tuple a b) where
    showsPrec n (a :* b) = showsPrec n a . showString "; " . showsPrec n b
