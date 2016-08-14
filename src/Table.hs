--module Table(Table,Tbl,newTable,findTable,updTable) where
module Table(Table,newTable,findTable,searchTable) where

data Table a b = L | T b a (Table a b) (Table a b) deriving (Eq,Show)

newTable    :: (Ord b) => [(b,a)] -> Table a b
newTable [] = L
newTable ((i,item):list) = insert (i,item) (newTable list) 
  where 
  insert (i,item) L = T i item L L 
  insert (i,item) (T j item' left right)
   | i == j =  T j item' left right 
   | i < j  =  T j item' (insert (i, item) left) right 
   | i > j  =  T j item' left (insert (i, item) right)

findTable :: (Ord b) => Table a b -> b -> a
findTable L i = error "item not found in table"
findTable (T j item left right) i
  | i == j   = item
  | i < j    = findTable left i
  | i > j    = findTable right i

--updTable    :: (Ord b) => (b,a) -> Table a b -> Table a b

searchTable :: Table a b -> (a -> Bool) -> [(b,a)]
searchTable L p = []
searchTable (T i item left right) p = 
  if p item then ((i,item):is) else is
  where is = (searchTable left p) ++ (searchTable right p)

-- vanaf hier start een specifieke invulling
  
type Nummer = Int
type Naam = String
data Geslacht = M | V deriving (Eq,Show)
data Status = Stud | AIO | Staf deriving (Eq,Show)
data Afdeling = Wisk | Inf | AI deriving (Eq,Show)

type DBitem = (Nummer,Naam,Geslacht,Status,Afdeling)

-- een aantal voorbeeld-items 

item1, item2, item3, item4 :: DBitem

item1 = (114, "Jan van Eijck", M, Staf, Inf)
item2 = (212, "Evan Goris", M, Stud, Inf)
item3 = (230, "Robbert de Haan", M, Stud, Wisk)
item4 = (173, "Annette Bleeker", V, AIO, Inf)

myDB = newTable [(1,item1),(2,item2),(3,item3),(4,item4)]

s1 = searchTable myDB (\(x,_,_,_,_) -> x > 200) 

s2 = searchTable myDB (\(_,_,_,stat,_) -> stat == Stud)

s3 = searchTable myDB (\(x,_,s,_,_) -> x > 11 && s == V)

