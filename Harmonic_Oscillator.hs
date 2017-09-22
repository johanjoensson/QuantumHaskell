import           MathLib.AutomaticDifferentiation
import           Data.Complex
import qualified Data.List                as Dat
import           Notation.DiracNotation

a :: Ket Int -> Ket Int
a = applyOp a_eig
    where
     a_eig KetZero = KetZero
     a_eig (Ket n) = sqrt (fromIntegral n) *| Ket (n - 1)

a' :: Ket Int -> Ket Int
a' = applyOp a_eig'
     where
      a_eig' KetZero = KetZero
      a_eig' (Ket n) = sqrt (fromIntegral $ n + 1 ) *| Ket (n + 1)

n :: Ket Int -> Ket Int
n   = a' . a

h :: Ket Int -> Ket Int
h x = n x + (1/2 *| x)

-- <x|n>
posState :: Double -> Ket Int -> Double
posState x k = val where
    D (val :+ _) _ = recursion (D (x :+ 0) (C (1.0 :+ 0))) k
    recursion _ KetZero   = D (0 :+ 0) (C (0 :+ 0))
    recursion x (Ket 0)   = (1/pi)**(1/4) * exp (negate x * x / 2.0)
    recursion x (Ket 1)   = sqrt 2.0 * x * recursion x (Ket 0)
    recursion x (k :+| l) = recursion x k + recursion x l
    recursion x (s :*| k) = C s * recursion x k
    recursion x k@(Ket n) = (recursion x (a $ a k)
                     - C (sqrt 2) * df (recursion x (a k))) / C (fromIntegral n)

hermite :: Double -> Ket Int -> Double
hermite x k = val where
    y = D (x :+ 0) (C (1.0 :+ 0))
    D (val :+ _) _ = exp (-y*y / 2.0) * herm y k
    herm y (Ket 0)   = C 1.0
    herm y (Ket 1)   = 2*y * herm y (Ket 0)
    herm y (k :+| l) = herm y k + herm y l
    herm y (s :*| k) = C s * herm y k
    herm y k@(Ket n) = 2.0 / sqrt (fromIntegral n) *
        ( y * herm y (a k) - fromIntegral n * herm y (a $ a k) /
        sqrt (fromIntegral (n - 1)))


main = do
    let u = Ket 3
    let v = a' u + (4.75*|u)
    let w = (0.25*|u) + (0.5*|v) + (0.32*|Ket 12)
    let psi = flip posState (normalize (w + v))
    let psiList = map psi [-8.0, -7.95 .. 8.0]
    let her = flip hermite (Ket 16)
    let herList = map her [-8.0, -7.95 .. 8.0]
    print $ zipWith (\x y -> (x, y)) [-8.0, -7.95 .. 8.0] psiList
