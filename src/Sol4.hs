module Sol4 

where 

import STAL
import Data.List
import SetEq

compare' :: Ord a => [a] -> [a] -> Ordering 
compare' [] [] = EQ
compare' (x:xs) (y:ys) | length (x:xs) < length (y:ys) = LT 
                       | length (x:xs) > length (y:ys) = GT
                       | otherwise = compare (x:xs) (y:ys) 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

splitList :: [a] -> [([a],[a])]
splitList [x,y] = [([x],[y])]
splitList (x:y:zs) = ([x],(y:zs)): addLeft x (splitList (y:zs)) 
   where addLeft u [] = []
         addLeft u ((vs,ws):rest) = (u:vs,ws): addLeft u rest 

split :: [a] -> [([a],[a])]
split [x,y] = [([x],[y])]
split (x:y:zs) = 
    ([x],(y:zs)) : (map (\ (us,vs) -> ((x:us),vs)) (split (y:zs)))

q11 = [ y | (x,y) <- act, x == "Robert De Niro" || x == "Kevin Spacey"]

q12 = nub ([ y | ("Quentin Tarantino",y) <- act, releaseP (y,"1994") ]
       ++  [ y | ("Quentin Tarantino",y) <- direct, releaseP (y,"1994") ])

q13 = [ x | (x,y) <- release, y > "1997", not (actP ("William Hurt",x)) ]

difference :: Eq a => [a] -> [a] -> [a]
difference xs []  = xs
difference xs (y:ys) = difference (delete y xs) ys 

genUnion :: Eq a => [[a]] -> [a]
genUnion [] = []
genUnion [xs] = xs
genUnion (xs:xss) = union xs (genUnion xss)

genIntersect :: Eq a => [[a]] -> [a]
genIntersect [] = error "list of lists should be non-empty"
genIntersect [xs] = xs
genIntersect (xs:xss) = intersect xs (genIntersect xss) 

unionSet :: (Eq a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) (deleteSet x set2))

intersectSet :: (Eq a) => Set a -> Set a -> Set a 
intersectSet (Set [])     set2  =  Set []
intersectSet (Set (x:xs)) set2 
   | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
   | otherwise    = intersectSet (Set xs) set2

differenceSet :: (Eq a) => Set a -> Set a -> Set a 
differenceSet set1 (Set []) = set1 
differenceSet set1 (Set (y:ys)) = 
   differenceSet (deleteSet y set1) (Set ys)

