{- integer partitions -} 

type Part = [Int]
type CmprPart = (Int,Part)

part :: Int -> [Part] 
part n | n < 1     = error "part: expects argument > 0"
       | otherwise = generatePs (0,[n])

generatePs :: CmprPart -> [Part] 
generatePs p@(n,[])     = [expand p]
generatePs p@(n,(1:xs)) = generatePs (n+1,xs)
generatePs p@(n,(x:xs)) = 
      (expand p: generatePs(nextpartition p))

nextpartition :: CmprPart -> CmprPart
nextpartition (n,(x:xs)) = pack (x-1) ((n+x),xs)

pack :: Int -> CmprPart -> CmprPart
pack n (m,xs) | n == 1    = (m,xs)
              | n > m     = pack (n-1) (m,xs) 
              | otherwise = pack n     (m-n,n:xs)

expand :: CmprPart -> Part
expand (0,p) = p
expand (n,p) = 1:(expand ((n-1),p))

{-
Main> part 5
[[5],[1,4],[2,3],[1,1,3],[1,2,2],[1,1,1,2],[1,1,1,1,1]]
Main> part 6
[[6],[1,5],[2,4],[1,1,4],[3,3],[1,2,3],[1,1,1,3],[2,2,2],
[1,1,2,2],[1,1,1,1,2],[1,1,1,1,1,1]]
Main> length (part 20)
627
Main> 
-}

{-  
The integer partitions of $n$ correspond to the sizes of the set
partitions of a set $A$ with $|A| = n$.

What is the {\bf order}\/ in which the integer partitions
are generated?  

Can you modify the generation algorithm so that it generates all the 
possible ways of giving coin change for amounts of money up to fl 10,-, 
in coins of fl 5, fl 2.50, fl 1, fl 0.25, fl 0.10 and fl 0.05? 
-}

