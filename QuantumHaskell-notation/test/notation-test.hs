module Main where

import           System.Exit            (exitFailure)

import           Data.Complex

import           Notation.DiracNotation

import           Test.QuickCheck        as QC
import          Test.Hspec as H

compKet :: Int -> Int -> Bool
compKet a b
    |a == b = ip == 1.0
    |otherwise = ip == 0.0
    where ip = realPart . abs $ toBra (Ket a) |.| Ket b

main = hspec $ do
   it "Check orthogonality condition" $ do 
    QC.quickCheck compKet
   it "Check addition of Kets\n|0> + |1> == |1> + |0>" $ do
    Ket 0 + (Ket 1) == Ket 1 + (Ket 0) `shouldBe` True
