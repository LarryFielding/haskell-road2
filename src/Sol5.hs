module Sol5

where 

import SetOrd
import Data.List
import REL

intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet (Set [])     set2  =  Set []
intersectSet (Set (x:xs)) set2 
   | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
   | otherwise    = intersectSet (Set xs) set2

restrictR :: Ord a => Set a -> Rel a -> Rel a
restrictR set rel = intersectSet (totalR set) rel 

rclosR :: Ord a => Rel a -> Rel a 
rclosR r = unionSet r (idR background)
  where background = unionSet (domR r) (ranR r)  

sclosR :: Ord a => Rel a -> Rel a 
sclosR r = unionSet r (invR r) 

tclosR :: Ord a => Rel a -> Rel a
tclosR r | transR r  = r 
         | otherwise = tclosR (unionSet r (r @@ r))

inDegree :: (Eq a) => Rel a -> a -> Int
inDegree (Set r) = \ x -> length [ y | (_,y) <- r, y == x ] 

outDegree :: (Eq a) => Rel a -> a -> Int
outDegree (Set r) = \ x -> length [ y | (y,_) <- r, y == x ] 

sources :: (Eq a) => Rel a -> Set a
sources (Set r) = Set [ x | x <- union (map fst r) (map snd r), 
                            inDegree  (Set r) x == 0, 
                            outDegree (Set r) x >= 1            ]

sinks :: (Eq a) => Rel a -> Set a
sinks (Set r) = Set [ x | x <- union (map fst r) (map snd r), 
                          outDegree (Set r) x == 0, 
                          inDegree  (Set r) x >= 1              ]

successor :: Rel' Int 
successor = \ n m -> n+1 == m

rel = unionR' successor (repeatR' [0..1000] successor 2)

transClosure' :: [a] -> Rel' a -> Rel' a
transClosure' xs r | transR' xs r = r 
                   | otherwise    = 
                     transClosure' xs (unionR' r (compR' xs r r))

rclass :: Rel' a -> a -> [a] -> [a]
rclass r x ys = [ y | y <- ys, r x y ]

bell :: Integer -> Integer
bell 0 = 1 
bell n = sum [stirling n k | k <- [1..n]]
  
stirling :: Integer -> Integer -> Integer 
stirling n 1             = 1
stirling n k | n == k    = 1
             | otherwise = k * (stirling (n-1) k) + stirling (n-1) (k-1)

listPartition :: Eq a => [a] -> [[a]] -> Bool
listPartition xs xss = 
     all (`elem` xs) (concat xss) && all (`elem` (concat xss)) xs
  && listPartition' xss [] 
  where 
  listPartition' []       _      = True 
  listPartition' ([]:xss) _      = False
  listPartition' (xs:xss) domain 
     | intersect xs domain == [] = listPartition' xss (union xs domain) 
     | otherwise                 = False 

listpart2equiv :: Ord a => [a] -> [[a]] -> Rel a
listpart2equiv dom xss 
  | not (listPartition dom xss) = error "argument not a list partition"
  | otherwise                   = list2set [(x,y) | xs<-xss, x<-xs, y<-xs]

equiv2listpart :: Ord a => Set a -> Rel a -> [[a]]
equiv2listpart s@(Set xs) r | not (equivalenceR s r) = 
  error "equiv2listpart: relation argument not an equivalence"
                            | otherwise              = 
  genListpart r xs
    where 
      genListpart r []     = []
      genListpart r (x:xs) = xclass : genListpart r (xs \\ xclass)
        where xclass = x : [ y | y <- xs, inSet (x,y) r ]

equiv2part :: Ord a => Set a -> Rel a -> Set (Set a) 
equiv2part s r = list2set (map list2set (equiv2listpart s r))

coins :: [Int]
coins = [1,2,5,10,20,50,100,200]

change :: Int -> [Int]
change n = moneyback n (n,[]) where 
    moneyback n (m,xs) | m == 0                  = xs
                       | n <= m && elem n coins  = moneyback n (m-n,n:xs)
                       | otherwise               = moneyback (n-1) (m,xs)

packCoins :: Int -> CmprPart -> CmprPart
packCoins k (m,xs) | k == 1                 = (m,xs)
                   | k <= m && elem k coins = packCoins k (m-k,k:xs)
                   | otherwise              = packCoins (k-1) (m,xs) 

nextCpartition :: CmprPart -> CmprPart
nextCpartition (k,(x:xs)) = packCoins (x-1) ((k+x),xs)

generateCps :: CmprPart -> [Part] 
generateCps p@(n,[])     = [expand p]
generateCps p@(n,(x:xs))  
        | elem x coins = (expand p: generateCps (nextCpartition p))
        | otherwise    = generateCps (nextCpartition p)

partC :: Int -> [Part] 
partC n | n < 1     = error "part: argument <= 0"
        | n == 1    = [[1]]
        | otherwise = generateCps (packCoins n (n,[]))

