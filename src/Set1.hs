module Set1 (Set,St,emptySet,setEmpty,inSet,subSet,addSet,delSet) where

import List

newtype Set a = St [a]

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

instance Eq a => Eq (Set a) where 
  set1 == set2 = subSet set1 set2 && subSet set2 set1 

instance (Show a) => Show (Set a) where
    showsPrec _ (St s) = showSet s

emptySet  :: Set a       
emptySet = St []

setEmpty  :: Set a -> Bool            
setEmpty (St []) = True
setEmpty _       = False

inSet     :: (Eq a) => a -> Set a -> Bool  
inSet x (St xs) = elem x xs

subSet :: Eq a => Set a -> Set a -> Bool
subSet (St []) _ = True
subSet (St (x:xs)) set = (inSet x set) && subSet (St xs) set

addSet    :: (Eq a) => a -> Set a -> Set a 
addSet x (St a) = St (x:a)

delSet    :: (Eq a) => a -> Set a -> Set a 
delSet x (St xs) = St (filter (/= x) xs)

