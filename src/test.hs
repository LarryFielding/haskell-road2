f :: Int -> Int 
f n = f (n+1)

h1 :: Integer -> Integer 
h1 0 = 0
h1 x = 2 * (h1 x) 

h2 :: Integer -> Integer 
h2 0 = 0
h2 x = h2 (x+1) 

h3 :: Integer -> Integer -> Integer 
h3 = \ x -> \ y -> x*y
h4 = \ x y -> x*y

catalan :: Integer -> Integer
catalan 0 = 1
catalan (n+1) = sum [ (catalan i) * (catalan (n-i)) | i <- [0..n] ]

catalan' :: Integer -> Integer
catalan' n = (binom (2*n) n) `div` (n+1)

binom n 0 = 1 
binom n k | n < k     = 0 
          | otherwise = (n * binom (n-1) (k-1)) `div` k 

