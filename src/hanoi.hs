data Peg = A | B | C 
type Tower = ([Int], [Int], [Int]) 

move :: Peg  -> Peg -> Tower -> Tower 
move A B (x:xs,ys,zs) =  (xs,x:ys,zs)
move B A (xs,y:ys,zs) =  (y:xs,ys,zs)
move A C (x:xs,ys,zs) =  (xs,ys,x:zs)
move C A (xs,ys,z:zs) =  (z:xs,ys,zs)
move B C (xs,y:ys,zs) =  (xs,ys,y:zs)
move C B (xs,ys,z:zs) =  (xs,z:ys,zs)

transfer :: Peg -> Peg -> Peg -> Int -> Tower -> [Tower]
transfer _ _ _ 0 tower = [tower] 
transfer p q r n tower = transfer p r q (n-1) tower
                         ++ 
                         transfer r q p (n-1) (move p q tower') 
   where tower' = last (transfer p r q (n-1) tower)

hanoi :: Int -> [Tower]
hanoi n = transfer A C B n ([1..n],[],[])


