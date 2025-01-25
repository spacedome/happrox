module MyLib (Cheb (Cheb), evalChebAtPoint) where
-- import qualified Data.Vector as V
import Numeric.Natural



-- Chebyshev is defined on the interval [-1,1]
-- We use Chebyshev polynomials of the first kind
newtype Cheb = Cheb {getCoef :: [Double]}
type ChebNodes = [Double]
newtype Function = Function {evaluate :: Double -> Double}

x :: Cheb
x = Cheb [1.0, 2.0]

-- SEE: https://hackage.haskell.org/package/math-functions-0.3.4.4/docs/Numeric-Polynomial-Chebyshev.html
evalChebAtPoint :: Cheb -> Double -> Double
evalChebAtPoint a x = fini . foldr step (0, 0) . tail $ getCoef a
    where step k (b0, b1) = (,) (k + 2 * x * b0 - b1) b0
          fini   (b0, b1) = head (getCoef a) + x * b0 - b1

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
