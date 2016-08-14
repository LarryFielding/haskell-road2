module SetEq (Set,St,emptySet,isEmpty,inSet,subSet,
             insertSet,deleteSet,powerSet,takeSet,
             (!!!),list2set) 
 
where

{-- Sets implemented as (unordered) lists without duplicates --} 

newtype Set a = St [a] deriving Eq

instance (Show a) => Show (Set a) where
    showsPrec _ (St s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' (shows x (showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a       
emptySet = St []

isEmpty  :: Set a -> Bool            
isEmpty (St []) = True
isEmpty _       = False

inSet  :: (Eq a) => a -> Set a -> Bool  
inSet x (St s) = elem x s

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (St []) _       = True  
subSet (St (x:xs)) set = (inSet x set) && subSet (St xs) set 

insertSet :: (Eq a) => a -> Set a -> Set a 
insertSet x (St ys) = St (insertList x ys) 

insertList :: (Eq a) => a -> [a] -> [a] 
insertList x ys | elem x ys = ys 
                | otherwise = (x:ys)

deleteSet :: Eq a => a -> Set a -> Set a 
deleteSet x (St xs) = St (deleteList x xs)

deleteList x [] = []
deleteList x (y:ys) | x == y    = deleteList x ys 
                    | otherwise = y : deleteList x ys 

list2set :: Eq a => [a] -> Set a
list2set [] = St []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = St (foldr insertList [] xs)

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (St xs) = St (map (\xs -> (St xs)) (powerList xs))

powerList :: Eq a => [a] -> [[a]]
powerList xs = power id xs []
  where 
  power f [] = ((f []):)
  power f (x:xs) = power f xs . power (f. (x:)) xs 

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (St xs) = St (take n xs) 

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a 
(St xs) !!! n = xs !! n
