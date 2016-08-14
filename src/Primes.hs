prime :: Integer -> Bool
prime n | n < 1     = error "argument not positive"
        | n == 1    = False 
        | otherwise = nofactors 2 n 
  where 
  nofactors m n | (m * m) > n  = True 
                | otherwise    = 
                  (rem n m) /= 0 && nofactors (m+1) n

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve (filter (\ n -> (rem n x) /= 0) xs)

primes :: [Integer]
primes = sieve [2..]

primePairs :: [(Integer,Integer)] 
primePairs = pairs primes 
  where 
  pairs (x:y:xys) | x + 2 == y = (x,y): pairs (y:xys)
                  | otherwise  = pairs (y:xys)

primeTriples :: [(Integer,Integer,Integer)]
primeTriples = triples primes 
  where 
  triples (x:y:z:xyzs) 
   | x + 2 == y && y + 2 == z = (x,y,z): triples (y:z:xyzs)
   | otherwise = triples (y:z:xyzs)

mersenne = [ (p,2^p - 1) | p <- primes, prime (2^p - 1) ]

divisors :: Integer -> [(Integer,Integer)]
divisors n | n < 0     = error "divisors called with negative argument"
           | otherwise = [ (d, quot n d)  | d <- [1..k], rem n d == 0 ] 
                where k = floor (sqrt (fromInteger n))

divs :: Integer -> [Integer]
divs n = (fst list) ++ reverse (snd list)
   where list = unzip (divisors n)

properDivs :: Integer -> [Integer]
properDivs n = init (divs n)

perfect :: Integer -> Bool 
perfect n = sum (properDivs n) == n 

prime' :: Integer -> Bool
prime' = \n -> divisors n == [(1,n)]

mersenne' = [ (p,2^p - 1) | p <- primes, prime' (2^p - 1) ]

notmersenne' = [ (p,2^p - 1) | p <- primes, not (prime' (2^p - 1)) ]

examples = [ take n primes | n <- [0..], 
                             not (prime (product (take n primes) + 1)) ]

