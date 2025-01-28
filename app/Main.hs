module Main where

import Approx.List

-- import qualified Data.Vector as V

-- x^4
chebX4 :: Cheb
chebX4 = Cheb [0.375, 0.0, 0.5, 0.0, 0.125]
x4 :: Function
x4 = Function (** 4)

main :: IO ()
main = do
  print (computeCheb x4 4)
