module Opdrw7 where

import List 

--1

fct2list :: (a -> b) -> [a] -> [(a,b)]
fct2list f xs = [ (x, f x) | x <- xs ]

--2 

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z) 

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d 
uncurry3 f (x,y,z) = f x y z 

--3 

comp :: Eq b => [(b,c)] -> [(a,b)] -> [(a,c)]
comp g f = [ (x,list2fct g y) | (x,y) <- f ] 

list2fct :: Eq a => [(a,b)] -> a -> b
list2fct [] _ = error "function not total"
list2fct ((u,v):uvs) x | x == u    = v
                       | otherwise = list2fct uvs x

--4 

fcts :: [a] -> [b] -> [[(a,b)]]
fcts []     _  = [[]]
fcts (x:xs) ys = [(x,y):f | y <- ys, f <- fcts xs ys ]

--5

fct2listpart :: (Eq a, Eq b) => (a -> b) -> [a] -> [[a]]
fct2listpart f [] = []
fct2listpart f (x:xs) = xclass : fct2listpart f (xs \\ xclass)
   where xclass =  x : [ y | y <- xs, f x == f y ]








