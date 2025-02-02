{-|
Module      : Approx.Cheb
Description : Chebyshev approximating polynomials
Copyright   : (c) Julien Brenneck 2025
License     : MIT
Maintainer  : julien@spacedome.tv
Stability   : experimental
Portability : POSIX

This module provides Chebyshev approximants backed by Vectors, using hmatrix.
-}

module Approx.Cheb where

import Numeric.GSL.Fourier
import Numeric.LinearAlgebra.Data
import Numeric.Natural
import qualified Data.Vector.Generic as V

{- Cheb - representation of our Chebyshev approximation.
 We store this using a sample at the extremal nodes, and not coefficients, following Chebfun.
 This has numerous benefits, and we can go bewteen representations as needed -}
newtype Cheb = Cheb {getNodes :: Vector R}
type ChebNodes = Vector R
type ChebCoefs = Vector R
-- | Function to sample from in computing a Cheb
newtype Function = Function {evalF :: R -> R}

-- | Compute the Nth extremal nodes, i.e. the interpolation points for our Cheb
extremalChebNodes :: Natural -> ChebNodes
extremalChebNodes n = build (fromIntegral n + 1) (\x -> cos (pi * x / fromIntegral n))

-- TODO: make size dynamic based on convergence
{- | Compute a Cheb representation of a Function. -}
computeCheb :: Function -> Natural -> Cheb
computeCheb f n = Cheb (cmap (evalF f) (extremalChebNodes n))


{- | Get the Chebyshev coefficients of a Cheb
This uses the FFT and is sometimes called the "Discrete Chebyshev Transform"

>> getChebCoef (computeCheb (Function (**4)) 4)
[0.575, 0.0, 0.5, 0.0, 0.125]
-}
getChebCoef :: Cheb -> ChebCoefs
getChebCoef (Cheb nodes) = filtered
  -- None of the literature seems to mention this, but computing the coefficients
  -- from the extremal nodes doesn't seem to work without this reflection trick
  where reflected = nodes <> (V.reverse . V.tail . V.init) nodes
        frequency = V.take (V.length nodes) ((cmap realPart . fft . complex) reflected)
        -- this FFT library does not normalize output, so we divide by N
        scaled    = cmap (/ fromIntegral (V.length frequency - 1)) frequency
        -- outermost points must be scaled by an additional factor of two
        scaled2   = scaled V.// [(0, 0.5 * V.head scaled), (V.length scaled - 1, 0.5 * V.last scaled)]
        -- might as well get rid of things near numerical zero, should improve stability
        filtered  = cmap (\x -> if abs x > 1e-14 then x else 0.0 ) scaled2

{- | Go from coefficients back to extremal node samples

>> inverseChebCoef (getChebCoef (computeCheb (Function (**4)) 4))
[1.0, 0.5, 0.0, 0.5, 1.0]
-}
inverseChebCoef :: ChebCoefs -> Cheb
inverseChebCoef coef = Cheb frequency
  where rescaled  = coef V.// [(0, 2.0 * V.head coef), (V.length coef - 1, 2.0 * V.last coef)]
        -- undo the scaling steps
        rescaled2 = cmap (* fromIntegral (V.length rescaled - 1)) rescaled
        -- do the reflection trick again (this works in both directions)
        reflected = rescaled2 <> (V.reverse . V.tail . V.init) rescaled2
        -- ifft and you're back
        frequency = V.take (V.length coef) ((cmap realPart . ifft . complex) reflected)


-- | Compute the first order Chebyshev differentiation matrix
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
    -- hmatrix `build` only takes R -> R -> R so we have to round...
    xi = x ! round i :: R
    xj = x ! round j :: R
    c z
      | z == 0 = 2
      | z == n = 2
      | otherwise = 1


data C = C {-# UNPACK #-} !R {-# UNPACK #-} !R
{- | Evaluate a Chebyshev polynomial of the first kind. 
Uses Clenshaw's algorithm.
Implementation taken from `math-functions` package (BSD3) -}
clenshaw :: ChebCoefs    -- ^ Coefficients of each polynomial term, in increasing order.
          -> R       -- ^ point to evalute at
          -> R
clenshaw a x = fini . V.foldr' stpp (C 0 0) . V.tail $ a
    where stpp k (C b0 b1) = C (k + x2 * b0 - b1) b0
          fini   (C b0 b1) = V.head a + x * b0 - b1
          x2               = x * 2
{-# INLINE clenshaw #-}
