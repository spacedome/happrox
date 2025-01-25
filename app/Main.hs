module Main where

import MyLib (Cheb, evalChebAtPoint)
-- import qualified Data.Vector as V

-- x^4
chebX4 = [0.375, 0.0, 0.5, 0.0, 0.125]


-- x
chebX1 = [0.0, 1.0]

main :: IO ()
main = do
  print (evalChebAtPoint chebX1 0.0)
  print (evalChebAtPoint chebX1 0.5)
  print (evalChebAtPoint chebX1 1.0)
  print (evalChebAtPoint chebX4 0.0)
  print (evalChebAtPoint chebX4 0.5)
  print (evalChebAtPoint chebX4 1.0)
