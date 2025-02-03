{-# LANGUAGE FlexibleContexts #-}

module Approx.Matrix where

import Numeric.LinearAlgebra
-- import System.Random (randomIO)
-- iport Control.Monad (replicateM)

-- -- | Generate a random Rademacher vector (+1/-1 with equal probability)
-- generateRademacherVector :: Int -> IO (Vector R)
-- generateRademacherVector n = do
--   -- Generate random bools and convert to ±1
--   signs <- replicateM n randomIO
--   return $ vector $ map (\b -> if b then 1 else -1) signs

-- {- | Estimate the trace using the Girard-Hutchinson estimator
-- Takes:
-- * Number of samples to use
-- * Size of the matrix
-- * A function that implements matrix-vector multiplication
-- Returns: The estimated trace
-- -}
-- estimateTrace ::
--   -- | Number of samples
--   Int ->
--   -- | Matrix dimension
--   Int ->
--   -- | Matrix-vector multiplication function
--   (Vector R -> Vector R) ->
--   -- | Estimated trace
--   IO R
-- estimateTrace numSamples n matVecMult = do
--   -- Generate random vectors and compute estimates
--   estimates <- replicateM numSamples $ do
--     v <- generateRademacherVector n
--     return $ v <.> matVecMult v
--   -- Average the estimates
--   return $ sum estimates / fromIntegral numSamples

normColumns :: Matrix Double -> Matrix Double
normColumns a =
  fromColumns $ fmap (\x -> x / scalar (norm_2 x)) (toColumns a)

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
    projSub v' us' = normalize $ foldl (-) v' (fmap (projV v') us')
