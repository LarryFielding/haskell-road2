module Set (Set,St,emptySet,isEmpty,inSet,subSet,
             insertSet,deleteSet,powerSet,takeSet,
            (!!!),list2set) where

import List

{-- Sets implemented as ordered lists without duplicates --} 

newtype Set a = St [a] deriving (Eq,Ord)

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

instance (Show a) => Show (Set a) where
    showsPrec _ (St s) str = showSet s str

emptySet  :: Set a       
emptySet = St []

isEmpty  :: Set a -> Bool            
isEmpty (St []) = True
isEmpty _       = False

inSet  :: (Ord a) => a -> Set a -> Bool  
inSet x (St s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (St []) _       = True  
subSet (St (x:xs)) set = (inSet x set) && subSet (St xs) set 

insertSet :: (Ord a) => a -> Set a -> Set a 
insertSet x (St s) = St (insertList x s) 

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 

deleteSet :: Ord a => a -> Set a -> Set a 
deleteSet x (St s) = St (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of 
                                 GT -> ys 
                                 EQ -> ys'
                                 _  -> y : deleteList x ys'

list2set :: Ord a => [a] -> Set a
--list2set [] = St []
--list2set (x:xs) = insertSet x (list2set xs)
list2set xs = St (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (St xs) = St (map list2set (powerList xs))

powerList :: Ord a => [a] -> [[a]]
powerList xs = power id (reverse xs) [] 
  where 
  power f [] = ((f []):)
  power f (x:xs) = power f xs . power (f . (x:)) xs 

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (St xs) = St (take n xs) 

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a 
(St xs) !!! n = xs !! n
