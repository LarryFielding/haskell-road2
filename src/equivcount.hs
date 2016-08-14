p :: Integer -> Integer
p 0 = 1 
p n = sum [p' n k | k <- [1..n]]
  where 
  p' 1 _ = 1 
  p' n k = k*(p' (n-1) k) + p' (n-1) (k-1)
