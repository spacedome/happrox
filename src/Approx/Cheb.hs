module Approx.Cheb where

import Numeric.GSL.Fourier
import Numeric.LinearAlgebra.Data
import Numeric.Natural

-- we typically represent using (extremal) nodes and not coefficients
-- this has numerous benefits, and we can go bewteen representations as needed
newtype Cheb = Cheb {getNodes :: Vector R}
type ChebNodes = Vector R
type ChebCoefs = Vector R
newtype Function = Function {evalF :: R -> R}

extremalChebNodes :: Natural -> ChebNodes
extremalChebNodes n = build (fromIntegral n + 1) (\x -> -cos (pi * x / fromIntegral n))

-- TODO: make size dynamic based on convergence
computeCheb :: Function -> Natural -> Cheb
computeCheb f n = Cheb (cmap (evalF f) (extremalChebNodes n))

getCoef :: Cheb -> ChebCoefs
-- NOTE: this is wrong
getCoef = cmap realPart . fft . complex . getNodes

chebDf :: Natural -> Matrix R
chebDf dim = build (m, m) f
 where
  m = fromIntegral dim + 1 :: Int
  n = fromIntegral dim :: R
  x = extremalChebNodes dim
  f :: R -> R -> R
  f i j
    | (i, j) == (0, 0) = (2 * n ** 2 + 1) / 6
    | (i, j) == (n, n) = -(2 * n ** 2 + 1) / 6
    | i == j = -xi / (2 * (1 - xi ** 2))
    | otherwise = (-1 ** (i + j)) * c i / (c j * (xi - xj))
   where
    xi = x ! round i :: R
    xj = x ! round j :: R
    c z
      | z == 0 = 2
      | z == n = 2
      | otherwise = 1

discreteChebTransform :: Cheb -> ChebCoefs
