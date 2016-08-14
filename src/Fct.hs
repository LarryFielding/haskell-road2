module Fct (list2fct, fct2list, restrict, image, coImage,
            injective, surjective, bijective, 
            untilFix)

where 

import List 

list2fct :: Eq a => [(a,b)] -> a -> b
list2fct [] _ = error "function not total"
list2fct ((u,v):uvs) x | x == u    = v
                       | otherwise = list2fct uvs x

fct2list :: (a -> b) -> [a] -> [(a,b)]
fct2list f xs = [ (x, f x) | x <- xs ]

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z) 

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d 
uncurry3 f (x,y,z) = f x y z 

restrict :: Eq a => (a -> b) -> [a] -> a -> b 
restrict f xs x | elem x xs = f x 
                | otherwise = error "argument not in domain"

restrictPairs :: Eq a => [(a,b)] -> [a] -> [(a,b)]
restrictPairs xys xs = [ (x,y) |  (x,y) <- xys, elem x xs ]

image :: Eq b => (a -> b) -> [a] -> [b] 
image f xs = nub [ f x | x <- xs ]

coImage :: Eq b => (a -> b) -> [a] -> [b] -> [a] 
coImage f xs ys = [ x | x <- xs, elem (f x) ys ] 

imagePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] 
imagePairs f xs = image (list2fct f) xs

coImagePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> [a] 
coImagePairs f xs ys = coImage (list2fct f) xs ys 

injective :: Eq b => (a -> b) -> [a] ->  Bool
injective f [] = True
injective f (x:xs) = notElem (f x) (image f xs) && injective f xs

surjective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
surjective f xs [] = True
surjective f xs (y:ys) = elem y (image f xs) && surjective f xs ys 

bijective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
bijective f xs ys = injective f xs && surjective f xs ys  

injectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] ->  Bool
injectivePairs f xs = injective (list2fct f) xs

surjectivePairs :: (Eq a, Eq b) =>  [(a,b)] -> [a] -> [b] -> Bool
surjectivePairs f xs ys = surjective (list2fct f) xs ys

bijectivePairs :: (Eq a, Eq b) =>  [(a,b)] -> [a] -> [b] -> Bool
bijectivePairs f xs ys = bijective (list2fct f) xs ys

comp :: Eq b => [(b,c)] -> [(a,b)] -> [(a,c)]
comp g f = [ (x,list2fct g y) | (x,y) <- f ] 

repeatF :: (a -> a) -> Int -> (a -> a) 
repeatF f n | n <= 0 = error "power must be positive"
repeatF f 1          = f 
repeatF f n          = f . (repeatF f (n-1))

untilFix :: Eq a => (a -> a) -> a -> a 
untilFix f x | x == f x  = x 
             | otherwise = untilFix f (f x)

-- the following function assumes that f is monotone increasing
untilLfix :: Ord a => (a -> a) -> a -> a 
untilLfix f x | x >  f x  = error "function not monotone increasing"
              | x == f x  = x 
              | otherwise = untilLfix f (f x)

-- the following function assumes that f is monotone decreasing
untilGfix :: Ord a => (a -> a) -> a -> a 
untilGfix f x | x <  f x  = error "function not monotone decreasing"
              | x == f x  = x 
              | otherwise = untilGfix f (f x)

ranPairs :: Eq b => [(a,b)] -> [b]
ranPairs f = nub [ y | (_,y) <- f ]

fcts :: [a] -> [b] -> [[(a,b)]]
fcts []     _  = [[]]
fcts (x:xs) ys = [(x,y):f | y <- ys, f <- fcts xs ys ]

l_0 = [(0,0),(1,0),(2,1)]
f_0 = list2fct l_0

fct2listpart :: (Eq a, Eq b) => (a -> b) -> [a] -> [[a]]
fct2listpart f [] = []
fct2listpart f (x:xs) = xclass : fct2listpart f (xs \\ xclass)
   where xclass =  x : [ y | y <- xs, f x == f y ]

listValues  :: Enum a => (a -> b) -> a -> [b]
listValues f i = (f i) : listValues f (succ i) 

listRange :: (Bounded a, Enum a) => (a -> b) -> [b]
listRange f = [ f i | i <- [minBound..maxBound] ]

