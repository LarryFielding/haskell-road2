compare2 :: Ord a => [a] -> [a] -> Ordering 
compare2 [] [] = EQ
compare2 (x:xs) (y:ys) | length (x:xs) < length (y:ys) = LT 
                       | length (x:xs) > length (y:ys) = GT
                       | otherwise = compare (x:xs)(y:ys) 