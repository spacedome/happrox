{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- This module has some list based algorithms
module Approx.List (Cheb (Cheb), extremalChebNodes, evalChebAtPoint, computeCheb, Function (Function)) where

import Data.Complex
import Numeric.Natural

-- Chebyshev is defined on the interval [-1,1]
-- We use Chebyshev polynomials of the first kind
newtype Cheb = Cheb {getCoef :: [Double]} deriving (Show)
type ChebNodes = [Double]
newtype Function = Function {evaluate :: Double -> Double}

extremalChebNodes :: Natural -> ChebNodes
extremalChebNodes n = [cos (pi * fromIntegral k / fromIntegral n) | k <- [0 .. 2 * n - 1]]

computeCheb :: Function -> Natural -> Cheb
computeCheb f n = Cheb (scaleCoef c)
 where
  y = fmap (evaluate f) (extremalChebNodes n)
  c = take (fromIntegral n + 1) (rfft y)
  scaleCoef :: [Double] -> [Double]
  scaleCoef [] = []
  scaleCoef (x : xs) = (x / (2 * fromIntegral n)) : go xs
   where
    go [] = []
    go [z] = [z / (2 * fromIntegral n)]
    go (z : zs) = (z / fromIntegral n) : go zs

-- adapted from the implementation in math-functions
-- SEE: https://hackage.haskell.org/package/math-functions-0.3.4.4/docs/Numeric-Polynomial-Chebyshev.html
-- Evaluate using Clenshaw algorithm
evalChebAtPoint :: Cheb -> Double -> Double
evalChebAtPoint a x = fini . foldr step (0, 0) . tail $ getCoef a
 where
  step k (b0, b1) = (,) (k + 2 * x * b0 - b1) b0
  fini (b0, b1) = head (getCoef a) + x * b0 - b1

-- FFT ---------------------------------------------------------------
split :: [a] -> ([a], [a])
split [] = ([], [])
split [_] = error "input size must be power of two"
split (x : y : xs) =
  let (es, os) = split xs
   in (x : es, y : os)

mergeRadix2 :: [Complex Double] -> [Complex Double] -> Int -> [Complex Double]
mergeRadix2 es os n = (++) (zipWith (+) es qs) (zipWith (-) es qs)
 where
  qs = zipWith (*) os ws
  ws = [exp (0.0 :+ (-2.0 * pi * fromIntegral k / fromIntegral n)) | k <- [0 .. length es - 1]]

fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [z] = [z]
fft zs = mergeRadix2 (fft evens) (fft odds) (length zs)
 where
  (evens, odds) = split zs

rfft :: [Double] -> [Double]
rfft = fmap realPart . fft . fmap (:+ 0.0)
