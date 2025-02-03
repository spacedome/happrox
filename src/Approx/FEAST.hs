{-# LANGUAGE BangPatterns #-}

module Approx.FEAST where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra as L
import System.Random (randomIO)
import Control.Monad (replicateM)

type MatrixC = Matrix C

-- Generate a random real matrix converted to complex with zero imaginary parts
randomRealMatrix :: Int -> Int -> IO (Matrix C)
randomRealMatrix n m = do
    reals <- replicateM (n*m) (randomIO :: IO Double)
    return $ complex (matrix n reals)

-- Generate quadrature points and weights for a circular contour
quadraturePoints :: R -> R -> Int -> [(C, C)]
quadraturePoints center radius nquad = zip zs ws
  where
    dtheta = 2 * pi / fromIntegral nquad :: R
    thetas = [ fromIntegral j * dtheta | j <- [0..nquad-1] ] :: [R]
    zs = [ (center :+ 0) + (radius :+ 0) * (cos theta :+ sin theta) | theta <- thetas ] :: [C]
    ws = [ ((radius :+ 0) * (cos theta :+ sin theta)) / fromIntegral nquad | theta <- thetas ] :: [C]

-- Compute the resolvent (zI - A)⁻¹X for each quadrature point and sum weighted by ws
computeY :: Matrix R -> [(C, C)] -> Matrix C -> Matrix C
computeY a quadPoints x = sum (map processQuad quadPoints)
  where
    n = rows a
    processQuad (z, w) = scalar w * (<\>) (mkResolvent z) (tr x)
    mkResolvent z = diag (konst z n) - complex a

-- Orthonormalize Y using QR decomposition
orthonormalize :: Matrix C -> Matrix C
orthonormalize y = q
  where (q, _) = qr y

-- Project A onto the orthonormal basis Q
projA :: Matrix C -> Matrix R -> Matrix C
projA q a = tr q L.<> complex a L.<> q

-- Compute Ritz vectors V = Q * xProj
computeRitzVectors :: Matrix C -> Matrix C -> Matrix C
computeRitzVectors q xProj = q L.<> xProj

-- Compute residuals A V - V Λ
residuals :: Matrix R -> Matrix C -> Vector R -> Matrix C
residuals a v lambdas = complex a L.<> v - v L.<> complex ( diag lambdas)

-- Compute the norms of the residuals for each Ritz vector
residualNorms :: Matrix C -> Vector R
residualNorms res = fromList $ map norm_2 (toColumns res)

-- Dense FEAST algorithm for real symmetric matrices
feast :: Matrix R -> R -> R -> Int -> Int -> Int -> R -> IO (Vector R)
feast a emin emax m0 nquad maxIter tol = do
    let n = rows a
        center = (emin + emax) / 2
        radius = (emax - emin) / 2 * 1.1  -- Slightly larger than the interval
        quadPoints = quadraturePoints center radius nquad
    -- Initial random subspace with real entries
    xInit <- randomRealMatrix n m0
    let iterateStep !x !iter = do
            let y = computeY a quadPoints x
                q = orthonormalize y
                aq = projA q a
                (lambdas, xProj) = eigSH (trustSym aq)
                v = computeRitzVectors q xProj
                res = residuals a v lambdas
                resNorms = residualNorms res
            putStrLn $ "Iteration " ++ show iter ++ ": max residual = " ++ show (maxElement resNorms)
            if maxElement resNorms < tol || iter >= maxIter
                then return lambdas
                else iterateStep v (iter + 1)
    iterateStep xInit 0

-- Example usage with a diagonal matrix
run :: IO ()
run = do
    let a = diag (fromList [1..50]) :: Matrix Double
        emin = 2.5
        emax = 4.5
        m0 = 2  -- Expect 2 eigenvalues in [2.5,4.5] (3 and 4)
        nquad = 8
        maxIter = 10
        tol = 1e-6
    eigenvals <- feast a emin emax m0 nquad maxIter tol
    putStrLn "Eigenvalues found:"
    print eigenvals
