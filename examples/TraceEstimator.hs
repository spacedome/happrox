module MatrixExamples where

import Approx.Matrix
import Numeric.LinearAlgebra

-- | Example usage with an explicit matrix
example :: IO ()
example = do
  -- Create a test matrix
  let n = 1000
  mat <- rand n n
  let matVecMult = (mat #>)

  -- Estimate trace with different numbers of samples
  trace10 <- estimateTrace 10 n matVecMult
  trace100 <- estimateTrace 100 n matVecMult
  trace1000 <- estimateTrace 1000 n matVecMult

  -- Calculate true trace for comparison
  let trueTrace = (konst 1 n :: Vector R) <.> (takeDiag mat)

  -- Print results
  putStrLn $ "True trace: " ++ show trueTrace
  putStrLn $ "Estimated trace (10 samples): " ++ show trace10
  putStrLn $ "Estimated trace (100 samples): " ++ show trace100
  putStrLn $ "Estimated trace (1000 samples): " ++ show trace1000

  -- Print relative errors
  let relError samples est = abs (est - trueTrace) / abs trueTrace
  putStrLn $
    "Relative error (10 samples): "
      ++ show (relError 10 trace10)
  putStrLn $
    "Relative error (100 samples): "
      ++ show (relError 100 trace100)
  putStrLn $
    "Relative error (1000 samples): "
      ++ show (relError 1000 trace1000)

gs :: IO ()
gs = do
  let a = (4 >< 4) [ 2, -1, 0, 0
                   , -1, 2, -1, 0
                   , 0, -1, 2, -1
                   , 0,  0, -1, 2] ::Matrix Double
  dispDots 2 $ gramSchmidt a
  dispDots 4 $ gramSchmidt a Numeric.LinearAlgebra.<> tr (gramSchmidt a)
