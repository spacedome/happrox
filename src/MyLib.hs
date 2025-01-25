module MyLib (Cheb, evalChebAtPoint) where
-- import qualified Data.Vector as V
import Numeric.Natural


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Chebyshev is defined on the interval [-1,1]
-- We use Chebyshev polynomials of the second kind, ie with extremal points
-- type Cheb = V.Vector Double
-- type ChebNodes = V.Vector Double
type Cheb = [Double]
type ChebNodes = [Double]
newtype Function = Function {evaluate :: Double -> Double}

-- chebyshevNodes :: Int -> ChebNodes
-- chebyshevNodes n = V.generate n (\k -> cos ((2 * fromIntegral k + 1) * pi / (2 * fromIntegral n)))

-- evalChebAtPoint :: Cheb -> Double -> Double
-- evalChebAtPoint coeffs x
--   | V.null coeffs = 0  -- No coefficients, return 0
--   | otherwise = go (V.length coeffs - 1) 0 0
--   where
--     -- Clenshaw algorithm
--     go k b1 b2
--       | k < 0     = b1  -- Base case: recursion complete
--       | otherwise = go (k - 1) (V.unsafeIndex coeffs k + 2 * x * b1 - b2) b1

-- SEE: https://hackage.haskell.org/package/math-functions-0.3.4.4/docs/Numeric-Polynomial-Chebyshev.html
evalChebAtPoint :: [Double] -> Double -> Double
evalChebAtPoint a x = fini . foldr step (0, 0) . tail $ a
    where step k (b0, b1) = (,) (k + 2 * x * b0 - b1) b0
          fini   (b0, b1) = head a + x * b0 - b1

computeCheb :: Function -> Natural -> Cheb
computeCheb = undefined
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
