prime :: Integer -> Bool 
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False 
        | otherwise = ld n == n where 
  ld  m   = ldf 2 m 
  ldf k m | rem m k == 0 = k 
          | k^2 >= m     = m
          | otherwise    = ldf (k+1) m

ldp :: Integer -> Integer 
ldp n = ldpf primes n 

ldpf :: [Integer] -> Integer -> Integer 
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n 
              | otherwise    = ldpf ps n

primes = 2 : filter prime [3..]

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p)  
   where p = ldp n

mersenne = [ (p,2^p - 1) | p <- primes, prime (2^p - 1) ]

notmersenne = [ (p,2^p - 1) | p <- primes, not (prime (2^p - 1)) ]

bertrand :: Integer -> (Integer, Integer)
bertrand n = (n, head (filter (> n) primes))

divisors :: Integer -> [Integer]
divisors n = [ m | m <- [1..n], rem n m == 0 ]
