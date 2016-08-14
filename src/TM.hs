module TM where 

import Galois2
import Polynomials

m  = One  : n 
m' = Zero : k
n  = Zero : (n^2 + (Zero: k^2))
k  = One  : (k^2 + (Zero: n^2))

t  = ones^2 + (Zero : Zero : Zero : t^4) where ones = One : ones 
t' = Zero : t^2

