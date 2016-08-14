module Power 

where

powerList :: [a] -> [[a]]
powerList xs = power id xs [] 

power ::  ([a] -> [a]) -> [a] -> [[a]] -> [[a]]
power f [] = ((f []):)
power f (x:xs) = power f xs . power (f . (x:)) xs 

