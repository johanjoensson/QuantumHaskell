import           Data.Complex
import           MathLib.MathFun
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

main = do
    print "Defining wavefunctions"
    let atomicWF = atomicWaveFunction 1 1
    let psi1 = atomicWF 5 4 4
    let psi2 = atomicWF 5 4 (-4)
    let psi r theta phi = 1/sqrt 2 *(psi1 r theta phi + psi2 r theta phi)
    print "Defining dataset!"
    let result = [( r * sin theta * cos phi,
                    r * sin theta * sin phi,
                    r *cos theta,
                    (r*r*sin theta :+ 0)* abs (psi r theta phi)**2)|
                    r <- [0, 1 .. 70],
                    theta <- [0, 0.05 .. pi],
                    phi <- [0, 0.1 .. 2*pi]]
    let stringList = map (\(x, y, z, n :+ _) -> show x ++ " " ++ show y ++ " "
                     ++ show z ++ " " ++ show n) result
    let y = sphericalHarmonics 5 0
    let int1 r t = fst (quad (integrand psi r t) 0 (2*pi))
    let int2 r = fst $ quad (\t -> sin t * int1 r t) 0 pi
    let int3 = fst $ quad2 (\r -> r*r * int2 r) 0
    print int3
    print "Printing dataset!"
    print "This is gonna take a while"
    f <- openFile "density.txt" WriteMode
    mapM_ (hPutStrLn f) stringList
    hClose f
    print "All done!"
