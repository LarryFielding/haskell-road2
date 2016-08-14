module Pow where 

import Set3 

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (St []) (St ys) = St ys
setUnion (St (x:xs)) (St ys) = addSet x (setUnion (St xs) (St ys))

pow :: (Ord a) => Set a -> Set [a]
pow (St [])     = St [[]]
pow (St (x:xs)) = setUnion pow_xs (addSet [x] pow_xs)  
                  where pow_xs = (pow (St xs))

-- does not quite give what we want, for all the sets have to be 
-- on the same level ...


union :: Eq a => [a] -> [a] -> [a]
union [] s     = s
union (x:xs) s | elem x s = union xs s
               | otherwise  = (x:(union xs s))

addElem :: Eq a => a -> [[a]] -> [[a]]
addElem x = map (union [x]) 

sublists  :: Eq a => [a] -> [[a]]
sublists  [] = [[]]
sublists  (x:xs) = union (sublists xs) (addElem x (sublists xs))
