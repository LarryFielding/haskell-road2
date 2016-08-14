module R 

where 
import List
import SetOrd

type Rel a = Set (a,a)

transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [ trans pair (Set s) | pair <- s ] 

trans :: Ord a => (a,a) -> Rel a  -> Bool
trans (x,y) (Set r) = 
       and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ] 
