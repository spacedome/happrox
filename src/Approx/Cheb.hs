module Approx.Cheb where

import Numeric.LinearAlgebra.Data
import Numeric.Natural

-- we typically represent using (extremal) nodes and not coefficients
-- this has numerous benefits, and we can go bewteen representations as needed
newtype Cheb = Cheb (Vector R)
type ChebNodes = Vector R

extremalChebNodes :: Natural -> ChebNodes
extremalChebNodes n = build (fromIntegral n + 1) (\x -> -cos (pi * x / fromIntegral n))
