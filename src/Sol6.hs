module Sol6

where 

import Data.Char
import Data.List
import SetOrd
import FCT

h' n = n * (n + 1)

k' n = n^2

l_0 = [(0,3),(1,2),(2,4),(3,2)]
f_0 = list2fct l_0
test_1 = fct2list (restrict f_0 [0,3]) [0,3]
test_2 = image f_0 [1,2,3]
test_3 = coImage f_0 [0,1,2,3] [2,4,5]

l_1 = [(0,0),(1,0),(2,1)]
f_1 = list2fct l_1

bijective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
bijective f xs ys = injective f xs && surjective f xs ys  

injectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] ->  Bool
injectivePairs f xs = injective (list2fct f) xs

surjectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
surjectivePairs f xs ys = surjective (list2fct f) xs ys

bijectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
bijectivePairs f xs ys = bijective (list2fct f) xs ys

injs :: [Int] -> [Int] -> [[(Int,Int)]]
injs [] xs = [[]]
injs xs [] = []
injs (x:xs) ys = 
  concat [ map ((x,y):) (injs xs (ys \\ [y])) | y <- ys ]

permut [] = [[]]
permut xs = [a:y | a <- xs, y <- permut(xs \\ [a])]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
  where 
  insrt :: a -> [a] -> [[a]]
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

comp :: Eq b => [(b,c)] -> [(a,b)] -> [(a,c)]
comp g f = [ (x,list2fct g y) | (x,y) <- f ] 

stringCompare :: String -> String -> Maybe Ordering 
stringCompare xs ys | any (not . isAlpha) (xs ++ ys) = Nothing 
                    | otherwise                      = Just (compare xs ys)

fct2listpart :: (Eq a, Eq b) => (a -> b) -> [a] -> [[a]]
fct2listpart f [] = []
fct2listpart f (x:xs) = xclass : fct2listpart f (xs \\ xclass)
   where xclass =  x : [ y | y <- xs, f x == f y ]

