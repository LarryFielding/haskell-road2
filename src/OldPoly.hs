module Polynomials 

where

z :: Num a => [a]
z = [0,1]

infixl 7 .*
(.*) :: Num a => a -> [a] -> [a]
c .* []     = []
c .* (f:fs) = c*f : c .* fs

instance Num a => Num [a] where
  fromInteger c   = [fromInteger c] 
  negate []       = []
  negate (f:fs)   = (negate f) : (negate fs)
  fs     + []     = fs
  []     + gs     = gs
  (f:fs) + (g:gs) = f+g : fs+gs
  fs     * []     = []
  []     * gs     = []
  (f:fs) * (g:gs) = f*g : (f .* gs + fs * (g:gs))

p2fct :: Num a => [a] -> a -> a
p2fct p x = sum (map (\ (a,n) -> a * x^n) (zip p [0..d]))
  where d  = (length p) - 1

comp :: Num a => [a] -> [a] -> [a]
comp []     _      = []
comp (f:fs) (0:gs) = f : gs * (comp fs gs)
comp (f:fs) (g:gs) = ([f] + [g] * (comp fs (g:gs)))
                   + (0 : gs * (comp fs (g:gs)))

deriv :: Num a => [a] -> [a]
deriv []     = []
deriv (f:fs) = deriv1 fs 1 where 
  deriv1 []     _ = []
  deriv1 (g:gs) n = n*g : deriv1 gs (n+1)


