module Main where

import Approx.Cheb
import Approx.List (extremalChebNodes)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

main :: IO ()
main = plotRunge

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
