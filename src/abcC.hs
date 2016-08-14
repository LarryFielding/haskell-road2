module Qdr where 

import Complex 

solveQdr :: (RealFloat a) => 
            (Complex a, Complex a, Complex a) -> (Complex a, Complex a) 
solveQdr (a,b,c) = -- if a = 0 then error "not quadratic" 
                   -- else 
                     let d = b^2 - 4*a*c in 
                     ((- b + sqrt d) / 2*a,
                      (- b - sqrt d) / 2*a)

