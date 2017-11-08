import           Data.Complex
import           MathLib.MathFun
import           Notation.QuantumState
import           Numeric.GSL.Integration
import           System.IO

radialWaveFunction :: (Integral a, Floating b) => a -> b -> a -> a -> b -> b
radialWaveFunction z mN n l r
                    |n >= 0 = c1 * exp ((-tau)/2*r) * tau**l' * r**l' *
                              generalizedLaguerre (n - l -1) (2*l' +1) (tau*r)
                    |otherwise = -1
                    where
                        l'  = fromIntegral l
                        n'  = fromIntegral n
                        z'  = fromIntegral z
                        mu  = mN*me/(mN + me)
                        aMu = me/mu
                        me  = 1
                        hb  = 1
                        c1  = sqrt $ tau**3 / (2*n')
                              / fromIntegral (product [n - l .. n + l])
                        tau = 2*z'/(n'*aMu)

atomicWaveFunction :: (Integral a, RealFloat b) => a -> b -> a -> a -> a -> b -> b ->
                  b -> Complex b
atomicWaveFunction z mN n l m r theta phi
            |n <= 0 = -5
            |l >= n = -5
            |m > l || m < -l= -5
            |otherwise  = (radialWaveFunction z mN n l r :+ 0) *
                           sphericalHarmonics l m theta phi

printReal :: (RealFloat a, Show a) => Complex a -> String
printReal (x :+ _) = show x

quad = integrateQNG 1E-16
quad1 = integrateQAGS 1E-16 10000
quad2 = integrateQAGIU 1E-16 10000

integrand :: RealFloat a => (a -> a -> a -> Complex a) -> a -> a -> a -> a
integrand f x y z = realPart (abs $ f x y z)**2

tripleIntegral :: (Double, Double) -> (Double, Double) -> (Double, Double) ->
                  (Double -> Double -> Double -> Double) -> Double
tripleIntegral (x0, x1) (y0, y1) (z0, z1) f
            | x1 > x0 && y1 > y0 && z1 > z0 = res
                where
                    res = fst $ quad2 int2 x0
                    int2 x = fst $ quad1 (int1 x) y0 y1
                    int1 x y = fst $ quad1 (f x y) z0 z1

data Tuple a b =
    a :* b
    deriving (Eq)

data Ket a =
    KetZero
    |Ket [a]
    |Scalar :*| Ket a
    |Ket a :+| Ket a

instance Eq a => QuantumState (Ket a)

main = do
    print "Defining wavefunctions"
    let atomicWF = atomicWaveFunction 1 1
    let psi1 = atomicWF 3 1 1
    let psi2 = atomicWF 2 1 (-1)
    print "Defining dataset!"
    let result = [( x, y, z,
                    (r*r*sin theta :+ 0)* (conjugate (psi2 r' theta' phi') * psi1 r theta phi) ) |
                    r <- [0, 0.1 .. 10],
                    theta <- [0, 0.05 .. pi],
                    phi <- [0, 0.1 .. 2*pi],
                    let x = r * sin theta * cos phi,
                    let y = r * sin theta * sin phi,
                    let z = r * cos theta,
                    let r' = sqrt $ (x-2)**2 + y**2 + z**2,
                    let theta' = acos (z/r'),
                    let phi' = atan2 y (x-2) ]
    let stringList = map (\(x, y, z, n :+ _) -> show x ++ " " ++ show y ++ " "
                     ++ show z ++ " " ++ show n) result

    let density r t p = conjugate (psi1 r t p) * psi2 r' t' p'
                        where
                            r' = sqrt $ (x-2)**2 + y**2 + z**2
                            t' = acos (z/r')
                            p' = atan2 y (x - 2)
                            x = r * sin t * cos p
                            y = r * sin t * sin p
                            z = r * cos t

    let integral = tripleIntegral (0, 5) (0, pi) (0, 2*pi)
                    (\r t p -> r*r * sin t * realPart (density r t p)) :+
                   tripleIntegral (0, 5) (0, pi) (0, 2*pi)
                    (\r t p -> r*r * sin t * imagPart (density r t p))
    let f = Ket [1 :* 'a']
    print integral
--    print "Printing dataset!"
--    print "This is gonna take a while"
--    f <- openFile "density.txt" WriteMode
--    mapM_ (hPutStrLn f) stringList
--    hClose f
    print "All done!"
