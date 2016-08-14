module Lazy where

import Polynomials
import PowerSeries

-- sqrtS :: Rational -> [Rational]
sqrtS y@(y0:_) = 
  iterate (\ x -> (1/2) * (x + y/x)) (sqrt y0)