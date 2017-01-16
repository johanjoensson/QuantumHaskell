import Data.Complex
import qualified Data.List as Dat
import DiracNotation

a :: Ket Int -> Ket Int
a n = apply_op a_eig n
    where 
     a_eig KetZero   = KetZero
     a_eig (Ket n)   = (sqrt $ fromIntegral n) *| (Ket (n - 1))

a' :: Ket Int -> Ket Int
a' n = apply_op a_eig' n
     where
      a_eig' KetZero  = KetZero
      a_eig' (Ket n)  = (sqrt $ fromIntegral $ n + 1 ) *| (Ket (n + 1))

n :: Ket Int -> Ket Int
n   = a' . a

h :: Ket Int -> Ket Int
h x = n x + (1/2 *| x)

main = do
    let u = Ket 0
    let v = a' u
    print $ h v
