import Polynomials
import PowerSeries
import COR
import POL


-- generating the positive naturals
pos = 1/(1-z)^2

-- generating the ones
one = 1/(1-z)

-- generating the naturals
nat =  z/(1-z)^2

-- generating the odd naturals
odd = (1+z)/(1-z)^2

-- generating [1,5] ++ repeat 6
-- one56s = 1 + 5*z + 6*z^2/(1-z)
-- one56s = 6/(1-z) - z - 5
one56s = [1,5] ++ repeat 6

-- generating [1,12,23] ++ repeat 24
one_12_23_24s = 1 + 12*z + 23 * z^2 +  24*z^3/(1-z)

-- generating [ n^4 | n <- [1..] ]
--fourths = psum $ psum $ psum $ psum one_12_23_24s

-- partial sum operation
-- psum fz = (1/(1-z)) * fz
psm :: Num a => [a] -> [a]
psm = psum 0 

psum ::  Num a => a -> [a] -> [a]
psum n [] = [n]
psum n (m:ks) = n : psum (n+m) ks
  

-- inverse of partial sum: difference with shift
dff fz = (1-z) * fz

-- generating the cubes
--cub = psum $ psum $ psum one56s

cr3 = (1 + 2*z)/(1-z^2) + (3 * z^2)/(1 - z^2 - z + z^3)

cr3' = (1/(1-z)) * cr3

cr3'' = cross2 cr3'

-- generating the cubes, Meissner style
cube = (1/(1-z)) * cr3''

square = (1+z)/(1-z)^3

--cross2 fz = (fz!!0) + z * cross2 (fz/z^2)  

cross2 (x:xs) = x: cross2 (tail xs)

cross3 (x:y:zs) = x: y: cross3 (tail zs)

cross n xs = take (n-1) xs ++ cross n (drop n xs)

-- pairsum 

pairsum (x:y:zs) = (x+y):pairsum zs

-- preceded by 0
ps xs = pairsum (0:xs)

-- law: 

-- cross 2 $ psum  == psum $ ps

trsum (x:y:z:us) = (x+y):z:trsum us

ts xs = trsum (0:xs)

-- cross 3 $ psum == psum $ ts 


pc :: Num a => Int -> Int -> [a] -> [a]
pc 1 _ xs = xs
pc n 1 (x:y:zs) = x+y: pc n (n-1) zs
pc n m (x:ys)   = x: pc n (m-1) ys


-- take 20 $psum $psum $ (pc 2 2) (cross 3 [1..])
-- gives the cubes

-- take 20 $ dff $ (pc 2 2) (cross 3 [1..])
-- [1,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]


-- take 20 $psum $psum $ (pc 2 2) $ psum $ (pc 3 3) (cross 4 [1..])
-- gives the fourth powers 

-- dff $ dff $ (pc 2 2) $ psum $ (pc 3 3) (cross 4 [1..]) 
--   = [1.0,12.0,23.0] ++ repeat 24.0


--dup :: Num a => [a] -> [a]
dup f = [f!!0 , f!!1] /(1 - z^2)
