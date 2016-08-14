gcde :: Integral a => a -> a -> (a,a,a) 
gcde a b | a == 0 && b == 0 = error "gcd undefined"
         | a < 0  || b < 0  = error "gcd undefined"
         | a == 0           = (b,0,1)
         |           b == 0 = (a,1,0)
         | a > b            = let 
                               (k,r)   = quotRem a b
                               (d,x,y) = gcde b r
                              in (d, y, x - k*y)
         | otherwise        = let
                               (k,r)   = quotRem b a
                               (d,x,y) = gcde a r
                              in (d, x - k*y, y)

