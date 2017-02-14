import           AutomaticDifferentiation
import           Data.Complex
import qualified Data.List                as Dat
import           DiracNotation

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
posState x n = val where
    D (val :+ _) _ = herm (D (x:+0) (C (1.0:+0))) n
    herm _ KetZero   = D 0 0
    herm x (Ket 0)   = exp(-x*x/2)
    herm x (Ket 1)   = 2.0*x*herm x (Ket 0)
    herm x (k :+| l) = herm x k + herm x k
    herm x (s :*| k) = C s * herm x k
    herm x (Ket n) = C (sqrt (fromIntegral (n - 1) / fromIntegral n)) * herm x (Ket (n-2)) - C (2/sqrt (fromIntegral n)) * df (herm x (Ket (n-2)))

main = do
    let u = Ket 15
    let v = a' u
    let w = (0.25*|u) + (0.5*|v) + (0.32*|(Ket 12))
    let psi = flip posState (normalize (Ket 0))
    print $ map psi [0, 0.1 .. 2.0]
