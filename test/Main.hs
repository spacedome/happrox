module Main (main) where
import Test.Tasty
import Test.Tasty.HUnit
import Chebyshev
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [clenshawTests, fftTests]

-- x^4
chebX4 :: Cheb
chebX4 = Cheb [0.375, 0.0, 0.5, 0.0, 0.125]
-- x
chebX1 :: Cheb
chebX1 = Cheb [0.0, 1.0]

testChebAtPoint :: Cheb -> String -> Double -> Double -> TestTree
testChebAtPoint cheb name x xe = 
  testCase tip $ assertBool "exceeds error bound" (err < 1e-12)
  where tip = name <> " at " <> show x
        err = abs (xe - evalChebAtPoint cheb x)

clenshawTests :: TestTree
clenshawTests = testGroup "Test evaluation at points (Clenshaw algorithm)"
  [ testChebAtPoint chebX1 "X1" 0.0 0.0,
    testChebAtPoint chebX1 "X1" 0.5 0.5,
    testChebAtPoint chebX1 "X1" (-1.0) (-1.0),
    testChebAtPoint chebX4 "X4" 0.0 0.0,
    testChebAtPoint chebX4 "X4" 0.5 0.0625,
    testChebAtPoint chebX4 "X4" (-1.0) 1.0
  ]

testChebCoef :: Function -> String -> Cheb -> TestTree
testChebCoef f tip (Cheb c) =
  testCase tip $ assertBool "exceeds error bound" (err < 1e-12)
  where
    (Cheb new) = computeCheb f (fromIntegral (length c - 1))
    err = maximum (fmap abs (zipWith (-) new c))

fftTests :: TestTree
fftTests = testGroup "Test computing Cheb coefficients with FFT"
  [ testChebCoef (Function (**4)) "X4" chebX4,
    testChebCoef (Function id) "X1" chebX1
  ]
