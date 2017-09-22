import           Data.Complex
import qualified Data.List    as Dat
import           Notation.QuantumState
--------------------------------------------------------------------------------
-- Infix operators for nice and simple notation                               --
--------------------------------------------------------------------------------
infixl 5 +| -- Addition of Kets
infixl 5 +< -- Addition of Bras

infix 6 *|  -- Scalar multiplication with Ket
infix 6 *<  -- Scalar multiplication with Bra

infixl 7 >| -- Ket tensor product
infixl 7 |< -- Bra tensor product

infix 4 |.| -- Inner product
-- infix 5 ><  -- Outer product


data Ket a =
    KetZero
    | Ket [a]
    | Scalar :*| Ket a
    | Ket a :+| Ket a

--------------------------------------------------------------------------------
-- Make our Ket vectors instances of the QuantumState type class              --
-- and define appropriate functions on Kets                                   --
--------------------------------------------------------------------------------
instance Eq a => QuantumState (Ket a) where
     add                = (+|)
     scale              = (*|)
     reduce             = reduceKet
     basis              = ketBasis
     components x       = [bracket e x | e <- basis x]
     compose coeffs v   = Dat.foldl1' (:+|) [uncurry (:*|) z | z <- zip coeffs v]
     norm KetZero = 0
     norm x       = sqrt $ realPart (bracketKet x  x)
     bracket            = bracketKet

reduceKet :: Eq a => Ket a -> Ket a
reduceKet x =
    compose coeff z
    where
        z = ketBasis x
        coeff = [bracketKet z x | z <- ketBasis x]


ketBasis :: Eq a => Ket a -> [Ket a]
ketBasis KetZero   = [KetZero]
ketBasis (Ket k)   = [Ket k]
ketBasis (_ :*| x) = ketBasis x
ketBasis (x :+| y) = Dat.nub (ketBasis x ++ ketBasis y)

bracketKet :: Eq a => Ket a -> Ket a -> Scalar
bracketKet KetZero _       = 0
bracketKet _ KetZero       = 0
bracketKet (s :*| x) y     = conjugate s * bracketKet x y
bracketKet x (s :*| y)     = s * bracketKet x y
bracketKet (x1 :+| x2) y=  bracketKet x1 y + bracketKet x2 y
bracketKet x (y1 :+| y2)=  bracketKet x y1 + bracketKet x y2
bracketKet (Ket a) (Ket b) = d a b

d :: Eq a => a -> a -> Scalar
d i j
    | i == j    = 1
    | otherwise = 0

instance Eq a => Eq (Ket a) where
    x == y = and [coeff v x == coeff v y | v <- ketBasis x]
        where
            coeff = bracketKet

(>|) :: Eq a => Ket a -> Ket a -> Ket a
KetZero >| _    = KetZero
_ >| KetZero    = KetZero
k >| l          = Dat.foldl1' (:+|) [bracketKet (Ket a) k * bracketKet (Ket b) l
                  *| Ket (a ++ b) | Ket a <- ketBasis k, Ket b <- ketBasis l]

(+|) ::Eq a => Ket a -> Ket a -> Ket a
KetZero +| x    = x
x +| KetZero    = x
x +| y
    | Dat.length xs == Dat.length ys = reduceKet (x :+| y)
    | otherwise = error "Trying to add two states of different length!"
    where
        Ket xs = head $ ketBasis x
        Ket ys = head $ ketBasis y

(*|) :: Eq a =>Scalar -> Ket a -> Ket a
_ *| KetZero    = KetZero
0 *| _          = KetZero
(0 :+ 0) *| _   = KetZero
s *| (x :+| y)  = s *| x +| s *| y
s *| (t :*| x)  = (s * t) *| x
s *| x          = s :*| x

--------------------------------------------------------------------------------
-- Define Bra vectors as the dual vectors of Kets                             --
--------------------------------------------------------------------------------
data Bra a = Bra { braBracket :: Ket a -> Scalar, toKet :: Ket a }
--------------------------------------------------------------------------------
-- Convert Ket vectors into Bra vectors                                       --
--------------------------------------------------------------------------------
toBra :: (Eq a) => Ket a -> Bra a
toBra k = Bra (bracketKet k) k

instance Eq a => QuantumState (Bra a) where
     add                = (+<)
     scale              = (*<)
     reduce             = reduceBra
     basis              = braBasis
     components x       = [bracket e x | e <- basis x]
     compose coeffs v   = toBra (Dat.foldl1' (:+|) [uncurry (:*|) z |
                          z <- zip coeffs (map toKet v)])
     norm x             = norm (toKet x)
     bracket            = bracketBra

(|<) :: Eq a => Bra a -> Bra a -> Bra a
x |< y  = toBra $ toKet x >| toKet y

(+<) :: Eq a => Bra a -> Bra a -> Bra a
x +< y  = toBra $ toKet x +| toKet y

(*<) :: Eq a => Scalar -> Bra a -> Bra a
s *< x = toBra $ s *| toKet x

reduceBra :: Eq a => Bra a -> Bra a
reduceBra x = toBra $ reduceKet (toKet x)

braBasis :: Eq a => Bra a -> [Bra a]
braBasis x = map toBra $ ketBasis (toKet x)

bracketBra :: Eq a => Bra a -> Bra a -> Scalar
bracketBra x y = braBracket x $ toKet y

(|.|) :: Eq a => Bra a -> Ket a -> Scalar
x |.| y = braBracket x y

creationOp :: (Integral a) => Int -> Ket a -> Ket a
creationOp _ KetZero    = KetZero
creationOp n (s :*| a)  = s *| creationOp n a
creationOp n (a :+| b)  = creationOp n a +| creationOp n b
creationOp n (Ket xs)
                    | occ >= 2  = KetZero
                    |otherwise  = c *| (Ket $ create' n xs)
                        where
                            create' i (y:ys)
                                | i == 0 = occ : ys
                                | otherwise = y : create' (i - 1) ys
                            c = sqrt $ fromIntegral occ
                            occ = (xs !! n) + 1

annihilationOp :: (Integral a) => Int -> Ket a -> Ket a
annihilationOp _ KetZero    = KetZero
annihilationOp n (s :*| a)  = s *| annihilationOp n a
annihilationOp n (a :+| b)  = annihilationOp n a +| annihilationOp n b
annihilationOp n (Ket xs)
                    | occ < 0   = KetZero
                    | otherwise = c *| (Ket $ annihilate' n xs)
                        where
                            occ = (xs !! n) - 1
                            annihilate' i (y:ys)
                                | i == 0 = occ : ys
                                | otherwise = y : annihilate' (i - 1) ys
                            c = sqrt $ fromIntegral (occ + 1)

occupancyOp :: (Integral a) => Int -> Ket a -> Ket a
occupancyOp _ KetZero    = KetZero
occupancyOp n (s :*| a)  = s *| occupancyOp n a
occupancyOp n (a :+| b)  = occupancyOp n a +| occupancyOp n b
occupancyOp n k@(Ket xs)
                    | occ < 0   = KetZero
                    | otherwise = fromIntegral occ *| k
                        where
                            occ = xs !! n

numberOp :: Integral a => Ket a -> Ket a
numberOp KetZero    = KetZero
numberOp (s :*| k)  = s *| numberOp k
numberOp (k :+| l)  = numberOp k +| numberOp l
numberOp k@(Ket xs) = n *| k
                    where
                        n = fromIntegral $ sum xs


a :: Integral a => Int -> Ket a -> Ket a
a = annihilationOp

a' :: Integral a => Int -> Ket a -> Ket a
a' = creationOp

occ :: Integral a => Int -> Ket a -> Ket a
occ = occupancyOp

num :: Integral a => Ket a -> Ket a
num = numberOp
--------------------------------------------------------------------------------
-- Make Ket a an instance of Show, in order to print Ket vectors in a pretty  --
-- way. Since Bra vectors are functions in Haskell they cannot be made an     --
-- instance of Show and thus cannot be printed                                --
--------------------------------------------------------------------------------
instance (Show a, Eq a) => Show (Ket a) where
    showsPrec _ KetZero   = showString "Zero-Ket"
    showsPrec _ (Ket [])  = showString "Zero-Ket"
    showsPrec _ (Ket j)   = showString "|" . showString (concatMap show j) . showString ">"
    showsPrec n (x :*| k) = showScalar n x . showsPrec n k
    showsPrec n (j :+| k) = showsPrec n j . showSign k . showsPrec n k

--------------------------------------------------------------------------------
-- Function to improve the prettyness of the printing.                        --
-- This function fixes the printing of negative coefficients.                 --
--------------------------------------------------------------------------------
showSign :: (Show a, Eq a) => Ket a -> String -> String
showSign (s@(a :+ b) :*| k)
    | b == 0, a < 0 = showString ""
    | otherwise = showString " + "
showSign (Ket j) = showString " + "

-- showStates :: (Show a) => [a] -> ShowS
-- showStates n = concatMap (showsPrec n)

main = do
    let a = Ket [1,1,0]
    let b = Ket [0,0,1]
    let c = a :+| b

    print a
