{- pair is the pairing function from Rogers, 
   Theory of Recursive Functions and Effective Computability. 

 Note that 
   div (x^2 + 2*x*y + y^2 + 3*x + y) 2             
   = (div ((x+y)*(x+y+1)) 2) + x
   = 1 + ... + (x + y) + x 
   which is the number of pairs that precede (x,y)
   if we define (m,n) < (x,y) 
   by means of m+n < x+y or (m+n=x+y and n < x).         -}


-- pair :: (Integer,Integer) -> Integer
pair :: (Int,Int) -> Int 
pair (x,y) | x < 0  || y  < 0 
           = error "pair called with negative argument"
           | otherwise 
           = div (x^2 + 2*x*y + y^2 + 3*x + y) 2

-- diagonalisation of N*N that goes with Rogers' pairing function: 

intpairs = [(x,z-x) | z <- [0..], x <- [0..z]]

-- projection functions that go with pairing function: 

proj1 :: Int -> Int
proj1 z | z < 0 = error "proj1--negative argument"
        | otherwise = fromInteger (fst (intpairs!!z))

proj2 :: Int -> Int
proj2 z | z < 0 = error "proj2--negative argument"
        | otherwise = fromInteger (snd (intpairs!!z))

