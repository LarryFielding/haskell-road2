generatePs :: (Int,[Int]) -> [[Int]] 
generatePs (n,[])       = [take n (repeat 1)]
generatePs (n,(x:xs))   = 
      (take n (repeat 1) ++ (x:xs)) : generatePs (pack (x-1) ((n+x),xs))
  where 
  pack :: Int -> (Int,[Int]) ->(Int,[Int])
  pack 1 (m,xs) = (m,xs)
  pack k (m,xs) = if k > m  then pack (k-1) (m,xs) 
                  else           pack k     (m-k,k:xs)

parts :: Int -> [[Int]] 
parts n | n < 1     = error "part: argument <= 0"
        | n == 1    = [[1]]
        | otherwise = generatePs (0,[n])

--Doaitse Swierstra

parts1 n = generate n n 
 
generate  m 0 = [[]]
generate  m n = [x:rest | x <- reverse [1..m], 
                          rest <- generate (x `min` (n-x)) (n-x)]

-- Tomasz Zielonka 

part :: Integer -> [[Integer]]
part = gen 1
    where
      gen m 0 = [[]]
      gen m n = [ x:xs | x <- [m..n], xs <- gen x (n - x) ]


