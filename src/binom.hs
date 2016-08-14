binom n k = (product [n-k+1..n]) `div` (product [1..k])

pascal n k | n < k  = 0
           | k == 0 = 1
           | otherwise = pascal (n-1) (k-1) + pascal (n-1) k
