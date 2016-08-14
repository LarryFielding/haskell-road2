genMatrix :: Int -> [Int] -> [[Int]]
genMatrix n xs = zipWith (++) (genM n) [ [x] | x <- xs ]
  where 
  genM n = [ [ x^(n-m) | m <- [0..n] ] | x <- [0..n] ]

