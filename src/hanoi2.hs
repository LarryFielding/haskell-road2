data Peg = A | B | C 
type Tower = ([Int], [Int], [Int]) 

ok :: [Int] -> Bool
ok [] = True 
ok (x:xs) = even x

target :: Int -> Tower -> Peg
target n (1:_,ys,zs) | odd n     = if ok zs then C else B
                     | otherwise = if ok ys then B else C
target n (xs,1:_,zs) = if ok zs then C else A 
target n (xs,ys,1:_) = if ok ys then B else A

move :: Peg  -> Peg -> Tower -> Tower 
move A B (x:xs,ys,zs) = (xs,x:ys,zs)
move B A (xs,y:ys,zs) = (y:xs,ys,zs)
move A C (x:xs,ys,zs) = (xs,ys,x:zs)
move C A (xs,ys,z:zs) = (z:xs,ys,zs)
move B C (xs,y:ys,zs) = (xs,ys,y:zs)
move C B (xs,ys,z:zs) = (xs,z:ys,zs)

move1 :: Int -> Tower -> Tower
move1 n t@(1:_,ys,zs) = move A (target n t) t
move1 n t@(xs,1:_,zs) = move B (target n t) t
move1 n t@(xs,ys,1:_) = move C (target n t) t

move2 :: Tower -> Tower 
move2 t@(1:xs,[],zs) = move C B t 
move2 t@(1:xs,ys,[]) = move B C t
move2 t@(1:xs,ys,zs) = if ys < zs then move B C t else move C B t
move2 t@([],1:ys,zs) = move C A t
move2 t@(xs,1:ys,[]) = move A C t
move2 t@(xs,1:ys,zs) = if xs < zs then move A C t else move C A t
move2 t@([],ys,1:zs) = move B A t
move2 t@(xs,[],1:zs) = move A B t 
move2 t@(xs,ys,1:zs) = if xs < ys then move A B t else move B A t

done :: Tower -> Bool
done ([],[], _) = True
done (xs,ys,zs) = False

transfer1, transfer2 :: Int -> Tower -> [Tower]
transfer1 n t = if done t then [t] else t : transfer2 n (move1 n t)
transfer2 n t = if done t then [t] else t : transfer1 n (move2 t) 

hanoi :: Int -> [Tower]
hanoi n = transfer1 n ([1..n],[],[]) 

zazen :: [Tower]
zazen = hanoi 64 
