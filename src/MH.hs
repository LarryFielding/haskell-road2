module MH where 

import Galois3
import Polynomials

m  = G1 : n 
m' = G0 : k
n  = G0 : alt n k 
k  = G1 : alt k n 

alt (x:xs) ys = x: alt ys xs 

t  = alt ones t' where ones = G1 : ones 
t' = alt zeros t where zeros = G0 : zeros

h = (1 + 2*z) * m
h'= (1 + 2*z) * m'


