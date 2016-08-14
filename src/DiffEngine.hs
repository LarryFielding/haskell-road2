module DiffEngine 

where 

import Matrix 

difs :: Integral a => [a] -> [a]
difs [] = []
difs [n] = []
difs (n:m:ks) = m-n : difs (m:ks)

difLists ::  Integral a => [[a]] -> [[a]]
difLists [] = []
difLists l@(xs:xss) = 
  if constant xs then l else difLists ((difs xs) : l)
  where 
  constant [] = error "not enough data or not a polynomial function"
  constant [n] = error "not enough data or not a polynomial function"
  constant (n:ms) = all (==n) ms

genDifferences :: Integral a => [a] -> [a]
genDifferences xs = map last (difLists [xs])


nextD :: Integral a => [a] -> [a]
nextD [] = error "no data"
nextD [n] = [n]
nextD (n:m:ks) = n : nextD (n+m : ks)

next :: Integral a => [a] -> a
next xs = (last . nextD . genDifferences) xs

continue :: Integral a => [a] -> [a]
continue xs = continue' differences 
  where 
  differences = genDifferences xs 
  continue' list = (last . nextD) list : continue' (nextD list) 

degree :: Integral a => [a] -> Int
degree xs = length (difLists [xs]) - 1

solve :: Int -> [Int] -> Matrix Int 
solve 0 (n:ns) = [[0,n]]
solve 1 (n0:n1:ns) = echelon [[0,n0],[1,1,n1]]
solve 2 (n0:n1:n2:ns) = echelon [[0,0,1,n0],
                                 [1,1,1,n1],
                                 [4,2,1,n2]]
solve 3 (n0:n1:n2:n3:ns) = echelon [[0,0,0,1,n0],
                                    [1,1,1,1,n1],
                                    [8,4,2,1,n2],
                                    [27,9,3,1,n3]]
solve 4 (n0:n1:n2:n3:n4:ns) = echelon [[0,0,0,0,1,n0],
                                       [1,1,1,1,1,n1],
                                       [2^4,2^3,2^2,2,1,n2],
                                       [3^4,3^3,3^2,3,1,n3],
                                       [4^4,4^3,4^2,4,1,n4]]
solve 5 (n0:n1:n2:n3:n4:n5:ns) = echelon [[0,0,0,0,0,1,n0],
                                          [1,1,1,1,1,1,n1],
                                          [2^5,2^4,2^3,2^2,2,1,n2],
                                          [3^5,3^4,3^3,3^2,3,1,n3],
                                          [4^5,4^4,4^3,4^2,4,1,n4],
                                          [5^5,5^4,5^3,5^2,5,1,n5]]
solve _ _ = error "degree > 5"

solveIt :: [Int] -> Matrix Int 
solveIt ns = solve (degree ns) ns 

{-

subst :: Matrix Int ->  [Ratio Int]
subst matrix = subst' matrix []
  where 
  subst' matrix ****
-} 

substit3 :: Matrix Int -> [Ratio Int]
substit3 rows = [a,b,c,d]
  where 
  d  = (p1 % q1) 
  p1 = (rows !! 3) !! 4
  q1 = (rows !! 3) !! 3
  c  = (p2 / q2) 
  p2 = (((rows !! 2) !! 4) % 1) -  (((rows !! 2) !! 3) % 1) * d
  q2 = ((rows !! 2) !! 2) % 1
  b  = (p3 / q3) 
  p3 =  (((rows !! 1) !! 4) % 1) 
          -  ((((rows !! 1) !! 2) % 1) * c 
              + ((((rows !! 1) !! 3) % 1) * d))
  q3 = ((rows !! 1) !! 1) % 1
  a  = (p4 / q4)
  p4 =  (((rows !! 0) !! 4) % 1) 
         - ((((rows !! 0) !! 1) % 1) * b
             + ((((rows !! 0) !! 2) % 1) * c)
             + ((((rows !! 0) !! 3) % 1) * d))
  q4 = ((rows !! 0) !! 0) % 1
