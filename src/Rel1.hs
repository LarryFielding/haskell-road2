module Rel1

where 

import SetOrd

type Rel a = Set (a,a)

idR :: Ord a => Set a -> Rel a 
idR (Set []) = (Set []) 
idR (Set (x:xs)) = insertSet (x,x) (idR (Set xs))

invR :: Ord a => Rel a -> Rel a
invR (Set []) = (Set [])
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y) = inSet (x,y) r 

reflR :: Ord a => Set a -> Rel a -> Bool
reflR (Set xs) r = subSet (Set [(x,x) | x <- xs]) r 

symR :: Ord a => Rel a -> Bool
symR (Set []) = True 
symR (Set ((x,y):pairs)) | x == y = symR (Set pairs)
                        | otherwise = 
                          inSet (y,x) (Set pairs) 
                          && symR (deleteSet (y,x) (Set pairs))

transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [ trans pair (Set s) | pair <- s ] where 
      trans (x,y) (Set r) = 
         and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ] 

union :: Ord a => Set a -> Set a -> Set a 
union (Set []) s = s
union (Set (x:xs)) s = insertSet x (union (Set xs) s) 

compR :: Ord a => Rel a -> Rel a -> Rel a 
compR (Set []) _ = (Set [])
compR (Set ((x,y):s)) r = 
   union (addcomp (x,y) r) (compR (Set s) r) 
   where 
   addcomp (x,y) (Set []) = (Set []) 
   addcomp (x,y) (Set ((u,v):r)) | y == u = 
                                   insertSet (x,v) (addcomp (x,y) (Set r))
                                 | otherwise = addcomp (x,y) (Set r)
                                  
repeatR :: Ord a => Rel a -> Int -> Rel a 
repeatR r n | n < 1     = error "argument < 1"
            | n == 1    = r 
            | otherwise = compR r (repeatR r (n-1))

