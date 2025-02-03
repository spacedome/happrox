{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-|
Module      : Approx.List
Description : Examples of approximation methods
Copyright   : (c) Julien Brenneck 2025
License     : MIT
Maintainer  : julien@spacedome.tv
Stability   : experimental
Portability : POSIX

This module includes pedagogical examples of approximation algorithms,
backed by Lists, without further dependencies, meant for clarity not performance.
-}

module Approx.List where

import Data.Complex
import Numeric.Natural

{- | A Chebyshev polynomial represented by coefficients
Chebyshev is defined on the interval [-1,1]
We use Chebyshev polynomials of the first kind -}
newtype Cheb = Cheb {getCoef :: [Double]} deriving (Show)
type ChebNodes = [Double]
newtype Function = Function {evalF :: Double -> Double}

-- | Compute the extremal (Gauss Labatto) Chebyshev nodes
extremalChebNodes :: Natural -> ChebNodes
extremalChebNodes n = [cos (pi * fromIntegral k / fromIntegral n) | k <- [0 .. 2 * n - 1]]

-- TODO: make this adaptive
-- TODO: maybe switch to sample representation like the Vector code
-- | Generate a Chebyshev representation of a Function
computeCheb :: Function -> Natural -> Cheb
computeCheb f n = Cheb (scaleCoef c)
 where
  y = fmap (evalF f) (extremalChebNodes n)
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
-- | Evaluate a Cheb at a point using Clenshaw algorithm
evalChebAtPoint :: Cheb -> Double -> Double
evalChebAtPoint a x = fini . foldr step (0, 0) . tail $ getCoef a
 where
  step k (b0, b1) = (,) (k + 2 * x * b0 - b1) b0
  fini (b0, b1) = head (getCoef a) + x * b0 - b1

-- FFT ---------------------------------------------------------------
-- | split into even and odd for FFT divide step
split :: [a] -> ([a], [a])
split [] = ([], [])
split [_] = error "input size must be power of two"
split (x : y : xs) =
  let (es, os) = split xs
   in (x : es, y : os)

-- | merge recursive radix-2 FFT steps
mergeRadix2 :: [Complex Double] -> [Complex Double] -> Int -> [Complex Double]
mergeRadix2 es os n = (++) (zipWith (+) es qs) (zipWith (-) es qs)
 where
  qs = zipWith (*) os ws
  ws = [exp (0.0 :+ (-2.0 * pi * fromIntegral k / fromIntegral n)) | k <- [0 .. length es - 1]]

-- | Compute the FFT of a signal of length 2**n
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [z] = [z]
fft zs = mergeRadix2 (fft evens) (fft odds) (length zs)
 where
  (evens, odds) = split zs

-- | Real part of the FFT 
rfft :: [Double] -> [Double]
rfft = fmap realPart . fft . fmap (:+ 0.0)
