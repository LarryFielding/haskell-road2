mp :: (a -> b) -> [a] -> [b]
mp f [] = [] 
mp f (x:xs) = f x : mp f xs

