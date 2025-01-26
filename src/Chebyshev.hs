{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Chebyshev (Cheb (Cheb), extremalChebNodes, evalChebAtPoint, computeCheb, Function (Function)) where
-- import qualified Data.Vector as V
import Numeric.Natural


import Data.Complex

split :: [a] -> ([a], [a])
split [] = ([], [])
split [_] = error "input size must be power of two"
split (x:y:xs) =
  let (es, os) = split xs
  in (x:es, y:os)

mergeRadix2 :: [Complex Double] -> [Complex Double] -> Int -> [Complex Double]
mergeRadix2 es os n = (++) (zipWith (+) es qs) (zipWith (-) es qs)
  where qs = zipWith (*) os ws
        ws = [exp (0.0 :+ (-2.0 * pi * fromIntegral k / fromIntegral n )) | k <- [0..length es -1]]

fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [z] = [z]
fft zs = mergeRadix2 (fft evens) (fft odds) (length zs)
  where (evens, odds) = split zs

rfft :: [Double] -> [Double]
rfft  = fmap realPart . fft . fmap (:+ 0.0)


-- Chebyshev is defined on the interval [-1,1]
-- We use Chebyshev polynomials of the first kind
newtype Cheb = Cheb {getCoef :: [Double]} deriving (Show)
type ChebNodes = [Double]
newtype Function = Function {evaluate :: Double -> Double}

extremalChebNodes :: Natural -> ChebNodes
extremalChebNodes n = [cos (pi * fromIntegral k / fromIntegral n) | k <- [0..2*n-1]]

computeCheb :: Function -> Natural -> Cheb
computeCheb f n = Cheb (scaleCoef c)
  where y = fmap (evaluate f) (extremalChebNodes n)
        c = take (fromIntegral n + 1) (rfft y)
        scaleCoef :: [Double] -> [Double]
        scaleCoef [] = []
        scaleCoef (x:xs) = (x / (2 * fromIntegral n)) : go xs
          where go [] = []
                go [z] = [z / (2 * fromIntegral n)]
                go (z:zs) = (z / fromIntegral n) : go zs
        


-- adapted from the implementation in math-functions
-- SEE: https://hackage.haskell.org/package/math-functions-0.3.4.4/docs/Numeric-Polynomial-Chebyshev.html
evalChebAtPoint :: Cheb -> Double -> Double
evalChebAtPoint a x = fini . foldr step (0, 0) . tail $ getCoef a
    where step k (b0, b1) = (,) (k + 2 * x * b0 - b1) b0
          fini   (b0, b1) = head (getCoef a) + x * b0 - b1

-- need to decide on which norm to use
approxError :: Function -> Cheb -> Double
approxError = undefined

-- These could be type classes ? idk probably no reason to
differentiate :: Cheb -> Cheb
differentiate = undefined
integrate :: Cheb -> Cheb
integrate = undefined

-- TODO: Functor to map over Cheb
-- TODO: Applicative to add or multiply two Cheb 
-- TODO: Parallel execution?
-- TODO: rootfinding
-- TODO: https://github.com/timbod7/haskell-chart
-- TODO: 
-- TODO: 
-- TODO: 
