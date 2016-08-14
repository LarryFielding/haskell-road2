module Decform

where 

import List

decExpand :: Rational -> [Integer]
decExpand x | r == 0    = [q]
            | otherwise = q : decExpand (abs (r*10) % d)
   where 
   (q,r) = quotRem n d
   n     = numerator x 
   d     = denominator x 

decForm :: Rational -> (Integer,[Int],[Int])
decForm x = (q,ys,zs) 
  where (q,r)   = quotRem n d 
        n       = numerator x
        d       = denominator x
        (ys,zs) = decF (abs (r*10)) d []

decF :: Integer -> Integer -> [(Int,Integer)] -> ([Int],[Int])
decF n d xs | r == 0        = (reverse (q: (map fst xs)),[])
            | elem (q,r) xs = (ys,zs) 
            | otherwise     =  decF (r*10) d ((q,r):xs)
     where 
     (q',r)  = quotRem n d
     q       = toInt q'
     xs'     = reverse xs
     Just k  = elemIndex (q,r) xs'
     (ys,zs) = splitAt k (map fst xs')