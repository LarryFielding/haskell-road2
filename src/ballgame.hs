ballgame :: [Integer] -> [[Integer]]
ballgame xs | all (==1) xs = [xs]
            | otherwise    = xs : ballgame (reduce xs)
    where 
    reduce []       = []
    reduce (1 : ys) = 1 : reduce ys
    reduce (n : ys) = (n-1) : (n-1) : ys
    
