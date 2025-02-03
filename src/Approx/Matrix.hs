{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Approx.Matrix
-- Description : Matrix algorithms
-- Copyright   : (c) Julien Brenneck 2025
-- License     : MIT
-- Maintainer  : julien@spacedome.tv
-- Stability   : experimental
-- Portability : POSIX
--
-- Some matrix algorithms of use in approximation theory and numerical analysis
module Approx.Matrix where

import Control.Monad (replicateM)
import Data.List (foldl')
import Numeric.LinearAlgebra
import System.Random (randomIO)

-- | Generate a random Rademacher vector (+1/-1 with equal probability)
generateRademacherVector :: Int -> IO (Vector R)
generateRademacherVector n = do
  -- Generate random bools and convert to ±1
  signs <- replicateM n randomIO
  return $ vector $ map (\b -> if b then 1 else -1) signs

-- | Estimate the trace using the Girard-Hutchinson estimator
estimateTrace ::
  -- | Number of samples
  Int ->
  -- | Matrix dimension
  Int ->
  -- | Mat-Vec
  (Vector R -> Vector R) ->
  -- | Estimated Trace
  IO R
estimateTrace numSamples n matVecMult = do
  -- Generate random vectors and compute estimates
  estimates <- replicateM numSamples $ do
    v <- generateRademacherVector n
    return $ v <.> matVecMult v
  -- Average the estimates
  return $ sum estimates / fromIntegral numSamples

-- | Normalize the columns of a matrix (in the 2-norm)
normColumns :: Matrix Double -> Matrix Double
normColumns a =
  fromColumns $ fmap (\x -> x / scalar (norm_2 x)) (toColumns a)

-- | Gram-Schmidt orthonormalization of a matrix
gramSchmidt :: Matrix Double -> Matrix Double
gramSchmidt a =
  fromColumns $ schmidt (toColumns a) []
  where
    schmidt :: [Vector Double] -> [Vector Double] -> [Vector Double]
    schmidt [] orthonormalVectors = reverse orthonormalVectors
    schmidt (v : vs) [] = schmidt vs [normalize v]
    schmidt (v : vs) us = schmidt vs (projSub v us : us)
      where
        projV :: Vector Double -> Vector Double -> Vector Double
        projSub :: Vector Double -> [Vector Double] -> Vector Double
        projV v' u' = u' * scalar (v' <.> u')
        projSub v' us' = normalize $ foldl' (-) v' (fmap (projV v') us')
