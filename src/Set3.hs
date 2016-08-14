module Set3 (Set,St,emptySet,setEmpty,inSet,subSet,
             addSet,delSet,list2set) where

import List

newtype Set a = St [a]

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

{--- Ordered List without Duplicates implementation ---}

instance (Show a) => Show (Set a) where
    showsPrec _ (St s) str = showSet s str

emptySet  :: Set a       
emptySet = St []

setEmpty  :: Set a -> Bool            
setEmpty (St []) = True
setEmpty _       = False

inSet  :: (Ord a) => a -> Set a -> Bool  
inSet x (St s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (St []) _ = True 
subSet (St (x:xs)) set = (inSet x set) && subSet (St xs) set 

addSet :: (Ord a) => a -> Set a -> Set a 
addSet x (St s) = St (add x s)
    where add x []                   = [x]                
          add x s@(y:ys)| (x>y)      = y : (add x ys)
                        | (x<y)      = x : s
                        | otherwise  = s

delSet :: Ord a => a -> Set a -> Set a 
delSet x (St s) = St (del x s)
    where del x []                   = []
          del x s@(y:ys)| (x>y)      = y : (del x ys)
                        | (x<y)      = s
                        | otherwise  = ys

list2set :: Ord a => [a] -> Set a
list2set [] = St []
list2set (x:xs) = addSet x (list2set xs)

