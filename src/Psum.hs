import POL
import Ratio
import Polynomials

psm :: [Integer] -> [Integer]
psm = psum 0 

psum :: Integer -> [Integer] -> [Integer]
psum n [] = [n]
psum n (m:ks) = n : psum (n+m) ks

infixr 8  ^-, ^+

-- falling powers, or falling factorials, or lower factorials
(^-) :: Integral a => a -> a -> a
x ^- 0 = 1
x ^- n = (x ^- (n-1)) * (x - n + 1)

-- rising powers, rising factorials, ascending factorials, upper factorials
(^+) :: Integral a => a -> a -> a
x ^+ 0 = 1
x ^+ n = (x ^+ (n-1)) * (x + n - 1)

-- newton polynomial: polynomial where the exponents
-- express falling factials

-- Newton's theorem gives a way to express a polynomial
-- of degree n as a newton polynomial, given its list 
-- for differences for f(0). 
--
-- we assume the differences are given as a list 
--   [D0, D1, ... , Dn]. 

newton :: (Fractional a, Enum a) => [a] -> [a]
newton xs = 
  [ x / product [1..fromInteger k] | (x,k) <- zip xs [0..] ]
  

firstDifs :: [Integer] -> [Integer]
firstDifs xs = reverse $ map head (difLists [xs])

list2npol ::  [Integer] -> [Double]
list2npol = newton . map fromInteger. firstDifs

-- stirlingC n k gives the number of ways in which a set of n
-- elements can be partitioned into k cycles

stirlingC :: Integer -> Integer -> Integer               
stirlingC 0 0 = 1
stirlingC 0 _ = 0
stirlingC n k = (n-1) * (stirlingC (n-1) k) + stirlingC (n-1) (k-1)


-- from falling powers to polynomial list representation
-- using Stirling cycle numbers

--fall2pol :: Integer -> [Integer]
fall2pol 0 = [1]
fall2pol n = 
   0 : [ (stirlingC n k) * (-1)^(n-k) | k <- [1..n] ]
   
-- from newton polynomials to polynomials:    

npol2pol :: Num a => [a] -> [a]
npol2pol xs = 
  sum [ [x] * (map fromInteger $ fall2pol k) | (x,k) <- zip xs [0..] ]
  
-- computing a polynomial from a sequence  

list2pol :: [Integer] -> [Double]
list2pol = npol2pol . list2npol