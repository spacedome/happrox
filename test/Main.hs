module Main (main) where
import Test.Tasty
import Test.Tasty.HUnit
import MyLib 
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- x^4
chebX4 :: Cheb
chebX4 = [0.375, 0.0, 0.5, 0.0, 0.125]
-- x
chebX1 :: Cheb
chebX1 = [0.0, 1.0] :: Cheb

testChebAtPoint :: Cheb -> String -> Double -> Double -> TestTree
testChebAtPoint cheb name x xe = 
  testCase tip $ assertBool "exceeds error bound" (err < 1e-12)
  where tip = name <> " at " <> show x
        err = abs (xe - evalChebAtPoint cheb x)

unitTests = testGroup "Unit tests"
  [ testChebAtPoint chebX1 "X1" 0.0 0.0,
    testChebAtPoint chebX1 "X1" 0.5 0.5,
    testChebAtPoint chebX1 "X1" (-1.0) (-1.0),
    testChebAtPoint chebX4 "X4" 0.0 0.0,
    testChebAtPoint chebX4 "X4" 0.5 0.0625,
    testChebAtPoint chebX4 "X4" (-1.0) 1.0
  ]
