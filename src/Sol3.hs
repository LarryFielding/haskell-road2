module Sol3  

where 

import TUOLP 

fasterprimes :: [Integer]
fasterprimes = 2 : sieve oddsFrom3

examples = [ take n primes | n <- [0..], 
                             not (prime (product (take n primes) + 1)) ]

