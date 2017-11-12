module Main where

import           System.Exit            (exitFailure)

import           Data.Complex

import           Notation.DiracNotation

import           Test.Hspec             as H

import           Test.QuickCheck        as QC

args = QC.stdArgs { maxSuccess = 5000 }

testInnerProd :: Double -> Double -> Double -> Double -> Int -> Int -> Bool
testInnerProd im1 re1 im2 re2 a b =
    ip1 == conjugate ip2
    where
        s1 = re1 :+ im1
        s2 = re2 :+ im2
        k1 = s1 *| Ket a
        k2 = s2 *| Ket b
        ip1 =toBra k1 |.| k2
        ip2 = toBra k2 |.| k1

testOrtho :: Int -> Int -> Bool
testOrtho i j
    | i == j = ip == 1.0
    | otherwise = ip == 0.0
    where
        b1 = toBra (Ket i)
        k2 = Ket j
        ip = b1 |.| k2

testBra :: Int -> Bool
testBra i = toKet b == k
    where
        k = Ket i
        b = toBra k

testLinear :: Int -> Int -> Bool
testLinear a b = k1 + k2 == k2 + k1
    where
        k1 = Ket a
        k2 = Ket b

testScalarMult :: Double -> Double -> Int -> Bool
testScalarMult im re i = k1 == k2
    where
        s = re :+ im
        k1 = s *| Ket i
        k2 = s :*| Ket i

testTensorMult :: Int -> Int -> Bool
testTensorMult i j = k1 == k2
    where
        k1 = Ket i >| Ket j
        k2 = Ket (i :* j)

main = hspec $ do
   it "Check complex conjugation of inner products" $ do
    QC.quickCheckWith args testInnerProd
   it "Check orthogonality of Kets" $ do
    QC.quickCheckWith args testOrtho
   it "Check linear combinations of Kets" $ do
     QC.quickCheckWith args testLinear
   it "Check scalar products with Kets" $ do
    QC.quickCheckWith args testScalarMult
   it "Check tensor product of Kets" $ do
    QC.quickCheckWith args testTensorMult
   it "Check Bra convertion of Kets" $ do
    QC.quickCheckWith args testBra
