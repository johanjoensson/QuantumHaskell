--------------------------------------------------------------------------------
-- This module contains amthematical functions useful for doing quantum       --
-- mechanics. All conventions are the "physics-version" of the mathematical   --
-- functions. No guarantee is given for the performance of these funcitons,   --
-- hopefully they won't be terribly bad. Numerical accuracy is also not       --
-- guaranteed. These functions should be correctly, if naively, implemented.  --
-- Johan Jönsson, 2017-09-17                                                  --
-- johanjoensson@gmail.com                                                    --
--------------------------------------------------------------------------------
module MathLib.MathFun
(sphericalHarmonics
, tesseralHarmonics
, associatedLegendre
, generalizedLaguerre
) where

import           Data.Complex

--------------------------------------------------------------------------------
-- Spherical Harmonics, including the Condon-Shortley phase.                  --
-- sphericalHarmonics take two integers, l and m, and two real numbers, theta --
-- and phi, and returns a complex number.                                     --
--------------------------------------------------------------------------------
sphericalHarmonics :: (Integral a, RealFloat b) => a -> a -> b -> b -> Complex b
sphericalHarmonics l m theta phi
                   |m < 0 && odd m  = conjugate $
                                   sphericalHarmonics l (-m) theta phi
                   |m < 0 && even m = negate $ conjugate $
                                   sphericalHarmonics l (-m) theta phi
                   |otherwise = cPhase m * clm *
                          (associatedLegendre l m (cos theta) :+ 0)*
                          exp ((0 :+ 1) * m' * (phi :+ 0))
                    where
                        m' = fromIntegral m
                        l' = fromIntegral l
                        cPhase i
                              |odd i = -1
                              |otherwise = 1
                        clm = sqrt $ (2*l' + 1)/(4 * pi)
                              / fromIntegral (product [l - m + 1 .. l + m])

--------------------------------------------------------------------------------
-- Tesseral harmonics, or a real basis for spherical harmonics                --
-- Just like sphericalHarmonics, tesseralHarmonics takes two integers,        --
-- l and m, and two real numbers, theta and phi, but returns a real number.   --
--------------------------------------------------------------------------------
tesseralHarmonics :: (Integral a, RealFloat b) => a -> a -> b -> b -> b
tesseralHarmonics l m theta phi = realPart (tH' l m)
                  where
                    tH' k n
                        |n < 0  = (0:+1)/sqrt 2 * (y k n -
                                  cPhase n * y k (-n))
                        |n > 0  = (1 :+ 0)/sqrt 2 * (y k (-n) +
                                  cPhase n * y k n)
                        |otherwise = y k 0
                    y k n   = sphericalHarmonics k n theta phi
                    cPhase i
                          | odd i   = -1
                          |otherwise= 1

--------------------------------------------------------------------------------
-- Associated Legendre polynomial, including the Condon-Shortley phase.       --
-- associatedLegendre takes two integers, l and m, and one real number, x,    --
-- and returns a real number                                                  --
--------------------------------------------------------------------------------
associatedLegendre :: (Integral a, Floating b) => a -> a -> b -> b
associatedLegendre l m x
    |m < 0      = cPhase m * fromIntegral (product [l+m +1 .. l-m])
                  * associatedLegendre l (-m) x
    |otherwise  = p' l m x
    where
        p' k i s
            |k < i  = 0
            |k - 1 == i = s * (2*i' + 1) * p' (k-1) i s
            |i == k = cPhase k * (1-s*s)**(i'/2) * dFact' (2*k - 1)
            |otherwise = recFac * (c1 * s * p' (k-1) i s - c2 * p' (k - 2) i s)
            where
                i'  = fromIntegral m
                k'  = fromIntegral l
                recFac  = 1 / (k' - i')
                c1      = 2*k' - 1
                c2      = k' + i' - 1
        dFact' n
            |n < 0 = 1
            |otherwise = fromIntegral $ product [y | y <- [1 .. n], odd y]
        cPhase i
            |odd i      = -1
            |otherwise  = 1

--------------------------------------------------------------------------------
-- Generalized Laguerre polynomial.                                           --
-- generalizedLaguerre takes one integer, n, one real number, l, and one      --
-- real number, x, and returns a real number.                                 --
--------------------------------------------------------------------------------
generalizedLaguerre :: (Integral a, Floating b) => a -> b -> b -> b
generalizedLaguerre n l x
                    |n >= 0 = lag' n l x
                    |otherwise = -1
                    where
                        lag' k a z
                            | k == 0    = 1
                            | k == 1    = 1 + a - z
                            |otherwise  = recFac * ((c1 - z) * lag' (k - 1) a z
                                          - c2 * lag' (k-2) a z)
                            where
                                k' = fromIntegral k
                                recFac = 1 / k'
                                c1 = 2*k' -1 + a
                                c2 = k' - 1 + a
