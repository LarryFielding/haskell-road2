gcde :: Integral a => a -> a -> (a,a,a) 
gcde 0 0   = error "gcd undefined"
gcde a b | a < 0  || b < 0 = error "gcd undefined"
gcde a 0 = (a,1,0)
gcde 0 a = (a,0,1)
gcde a b | a > b = (d, y, x - k*y)
  where 
     (k,r)   = quotRem a b
     (d,x,y) = gcde b r
gcde a b | a < b = (d, x - k*y,y)
  where 
     (k,r)   = quotRem b a
     (d,x,y) = gcde a r


