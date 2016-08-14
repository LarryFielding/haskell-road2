module Sol7  

where 

import Data.List
import IAR

qrm :: Natural -> Natural -> (Natural,Natural)
qrm m n | gt n m    = (Z,m)
        | otherwise = (S (fst qr), snd qr) where qr = qrm (subtr m n) n

quotient ::  Natural -> Natural -> Natural 
quotient m n  = fst (qrm m n)

remainder ::  Natural -> Natural -> Natural 
remainder m n  = snd (qrm m n)

pre :: Natural -> Natural
pre Z = Z
pre (S n) = n
 
subtr :: Natural -> Natural -> Natural
subtr = foldn pre   

catalan :: Integer -> Integer
catalan 0     = 1
catalan n = sum [ (catalan i) * (catalan (n-1-i)) | i <- [0..n] ]

data TernTree = L' | N' TernTree TernTree TernTree deriving Show

makeTernTree :: Integer -> TernTree 
makeTernTree 0 = L'
makeTernTree n = N' 
   (makeTernTree (n-1)) (makeTernTree (n-1)) (makeTernTree (n-1))

count3 :: TernTree -> Integer
count3 L'            = 1
count3 (N' t1 t2 t3) = 1 + count3 t1 + count3 t2 + count3 t3

insertTree :: Int -> Tree -> Tree
insertTree n Lf = (Nd n Lf Lf)
insertTree n t@(Nd m left right) 
     | m < n     = Nd m left (insertTree n right)
     | m > n     = Nd m (insertTree n left) right
     | otherwise = t

list2tree :: [Int] -> Tree 
list2tree [] = Lf 
list2tree (n:ns) = insertTree n (list2tree ns)

tree2list :: Tree -> [Int]
tree2list Lf = []
tree2list (Nd n left right) = tree2list left ++ [n] ++ tree2list right

inTree :: Int -> Tree -> Bool
inTree n Lf = False 
inTree n (Nd m left right) | n == m = True 
                           | n <  m = inTree n left 
                           | n >  m = inTree n right 

mergeTrees :: Tree -> Tree -> Tree
mergeTrees t1 t2 = foldr insertTree t2 (tree2list t1)

findDepth :: Int -> Tree -> Int 
findDepth _ Lf = -1 
findDepth n (Nd m left right) 
    | n == m = 0 
    | n <  m = if d1 == -1 then -1 else d1 + 1
    | n >  m = if d2 == -1 then -1 else d2 + 1
  where d1 = findDepth n left 
        d2 = findDepth n right 

mapT ::  (a -> b) -> Tr a -> Tr b
mapT f Nil = Nil
mapT f (T x left right) = T (f x) (mapT f left) (mapT f right)

foldT :: (a -> b -> b -> b) -> b -> (Tr a) -> b
foldT h c Nil = c 
foldT h c (T x left right) = h x (foldT h c left) (foldT h c right)

preorderT, inorderT, postorderT :: Tr a -> [a]
preorderT = foldT preLists [] 
  where preLists x ys zs = (x:ys) ++ zs
inorderT = foldT inLists [] 
  where inLists x ys zs = ys ++ [x] ++ zs
postorderT = foldT postLists [] 
  where postLists x ys zs = ys ++ zs ++ [x]

orderedT :: Ord a => Tr a -> Bool
orderedT tree = ordered (inorderT tree) 
  where 
  ordered []   = True
  ordered [_]  = True 
  ordered (x:y:zs) = x <= y && ordered (y:zs) 

orderedT' :: Ord a => Tr a -> Bool
orderedT' tree = and (zipWith (<) (x:xs) xs)
  where (x:xs) = inorderT tree

lookupD :: String -> Dict -> [String]
lookupD _ Nil = []
lookupD x (T (v,w) left right) | x == v    = [w]
                               | x < v     = lookupD x left
                               | otherwise = lookupD x right

buildTree :: [a] -> Tr a
buildTree [] = Nil 
buildTree xs = T m (buildTree left) (buildTree right) 
   where (left,m,right) = split xs 

mapLT :: (a -> b) -> LeafTree a -> LeafTree b 
mapLT f (Leaf x) = Leaf (f x) 
mapLT f (Node left right) =  Node (mapLT f left) (mapLT f right) 

reflect :: LeafTree a -> LeafTree a
reflect (Leaf x) = Leaf x
reflect (Node left right) = Node (reflect right) (reflect left)

mapR :: (a -> b) -> Rose a -> Rose b
mapR f (Bud x)  = Bud (f x) 
mapR f (Br roses) = Br (map (mapR f) roses)

genUnion :: Eq a => [[a]] -> [a]
genUnion = foldr union []

genIntersect :: Eq a => [[a]] -> [a]
genIntersect = foldr1 intersect 

insrt :: Ord a => a -> [a] -> [a]
insrt x [] = [x]
insrt x (y:ys) = if x <= y then (x:y:ys) else y : (insrt x ys) 

srt :: Ord a => [a] -> [a]
srt = foldr insrt []

ln' :: [a] -> Natural
ln' = foldl (\ x _ -> S x) Z

compareT :: Tower -> Tower -> [Ordering]
compareT t t' | maxT t < maxT t' = [ LT | checkT t && checkT t' ]
              | maxT t > maxT t' = [ GT | checkT t && checkT t' ]
              | otherwise        = [ compare' t t' | checkT t && checkT t' ]

compare' :: Tower -> Tower -> Ordering
compare' ([],[],[]) ([],[],[])   = EQ
compare' t@(xs,ys,zs) t'@(xs',ys',zs') 
  | firstStage t && firstStage t' = 
    compare' (init xs, zs, ys) (init xs', zs', ys')
  | lastStage t  && lastStage t'  = 
    compare' (ys, xs, init zs) (ys', xs', init zs') 
  | firstStage t && lastStage t'  = LT 
  | lastStage t  && firstStage t' = GT 
  where 
  firstStage (xs,ys,zs) = xs /= [] && last xs == maxT t 
  lastStage  t = not (firstStage t) 

hanoi'' :: Int -> [Tower] 
hanoi'' n = [ hanoiCount n k | k <- [0..2^(toInteger n)-1] ] 

fromTower :: Tower -> Integer 
fromTower t = (2^n - 1) + (fromT t n) where 
  n = maxT t 
  fromT (xs,ys,zs) k 
    | xs == [1..k] = 0 
    | elem k xs    = fromT  (init xs, zs, ys) (k-1) 
    | elem k zs    = 2^(k-1) +  fromT  (ys, xs, init zs) (k-1)
    | otherwise    = error "not a proper tower configuration"

