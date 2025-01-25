module Main where

import MyLib (Cheb, evalChebAtPoint)
-- import qualified Data.Vector as V

-- x^4
chebX4 :: Cheb
chebX4 = [0.375, 0.0, 0.5, 0.0, 0.125]

main :: IO ()
main = do
  print (evalChebAtPoint chebX4 1.0)
