module Main where

import Approx.Cheb
import Approx.List (extremalChebNodes)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

main :: IO ()
main = plotDerivative

-- plot a Chebyshev interpolation of the classic function
-- used to demonstrate Runge's phenomenon.
plotRunge :: IO ()
plotRunge = toFile def "runge.png" $ do
  layout_title .= "1 / (1 + 25x^2)"
  setColors [opaque blue, opaque green, opaque red]
  plot (line "f" [zip linrange (fmap runge linrange)])
  plot (line "cheb14" [zip linrange interp])
  plot (points "points" (zip nodes (fmap runge nodes)))
  where
    linrange = [x / 100 | x <- [-100 .. 100]] :: [Double]
    runge x = 1 / (1 + 25 * x ** 2)
    rungeCheb = computeCheb (Function runge) 14
    interp = fmap (clenshaw (getChebCoef rungeCheb)) linrange
    nodes = Approx.List.extremalChebNodes 14

    
plotDerivative :: IO ()
plotDerivative = toFile def "deriv.png" $ do
  layout_title .= "derivative"
  setColors [opaque blue, opaque green, opaque red, opaque orange, opaque black]
  plot (line "f" [zip linrange func])
  plot (line "f'" [zip linrange (fmap f' linrange)])
  plot (line "f''" [zip linrange (fmap f'' linrange)])
  plot (line "cheb f'" [zip linrange func'])
  plot (line "cheb f''" [zip linrange func''])
  where
    linrange = [x / 100 | x <- [-100 .. 100]] :: [Double]
    f x = 1 / (1+16 * x**2)
    f' x = -32 * x / (1 + 16 * x **2) ** 2
    f'' x = -32 * (1 - 48 * x**2)/(1 + 16 * x**2)**3
    c = computeCheb (Function f) 14
    func = fmap (clenshaw (getChebCoef c)) linrange
    func' = fmap (clenshaw (getChebCoef (diffCheb c))) linrange
    func'' = fmap (clenshaw (getChebCoef (diffCheb $ diffCheb c))) linrange
