-- Iene Miene Mutte 

telaf :: Int -> Int -> Int 
telaf d n = tellen d 1 (cycle [0..(n-1)]) 

tellen :: Int -> Int -> [Int] -> Int 
tellen d n (x:y:zs) | x == y    = x
                    | n == d    = tellen d 1 (filter (/= x) (y:zs))
                    | otherwise = tellen d (n+1) (y:zs) 

-- slimme versie 

f :: Int -> Int -> Int 
f d 1 = 0
f d n = ((f d (n-1)) + (d `mod` n)) `mod` n

-- coincidence count (neem aan dat beide lijsten stijgend zijn) 

cc :: [Int] -> [Int] -> Int
cc _ [] = 0
cc [] _ = 0
cc (x:xs) (y:ys) | x < y  = cc xs (y:ys)
                 | x > y  = cc (x:xs) ys 
                 | x == y = 1 + cc xs ys 


