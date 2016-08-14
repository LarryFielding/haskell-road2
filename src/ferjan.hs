{--  From: Infinitary Rewriting, Kennaway and De Vries  --}

step1 n = n : step1 (n+1)
step2 n = n : step2 (n+2)

nats = step1 2 
odds = step2 3

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark (xs, n-1, n-1))
  where 
  mark (x : xs, 0, m) = 0 : mark (xs, m, m)
  mark (x : xs, n, m) = x : mark (xs, n-1, m) 

primes = sieve [2..]

fasterprimes :: [Integer]
fasterprimes = 2 : sieve (odds) 

