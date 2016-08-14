-----------------------------------------------------------------
--                                                             --
-- Uitwerkingen van de opdrachten voor Week 4                  --
--                                                             --
-----------------------------------------------------------------

module W4  where 

import TAMO
import TUOLP

{- 2.13 -} 

tst1a = not True <=> False
tst1b = not False <=> True
tst2  = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
tst3a = logEquiv1 (\ p -> p || True) (const True)
tst3b = logEquiv1 (\ p -> p && False) (const False)
tst4a = logEquiv1 (\ p -> p || False) id
tst4b = logEquiv1 (\ p -> p && True) id 
tst5  = logEquiv1 excluded_middle (const True)
tst6  = logEquiv1 (\ p -> p && not p) (const False)

{- 2.15 -}

contrad1 :: (Bool -> Bool) -> Bool
contrad1 bf =  not (bf True) && not (bf False)

contrad2 :: (Bool -> Bool -> Bool)  -> Bool
contrad2 bf = and [not (bf p q) | p <- [True,False], q <- [True,False]]

contrad3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contrad3 bf = and [ not (bf p q r) | p <- [True,False], 
                                     q <- [True,False], 
                                     r <- [True,False]] 


{- 2.51 -} 

unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

{- 3.39 -} 

examples = [ take n primes | n <- [0..], 
                             not (prime (product (take n primes) + 1)) ]

{- Dit genereert: 

[[2,3,5,7,11,13],
 [2,3,5,7,11,13,17],
 [2,3,5,7,11,13,17,19],
 [2,3,5,7,11,13,17,19,23],
 [2,3,5,7,11,13,17,19,23,29],
 [2,3,5,7,11,13,17,19,23,29,31,37],
 [2,3,5,7,11,13,17,19,23,29,31,37,41],
 [2,3,5,7,11,13,17,19,23,29,31,37,41,43],
 [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47],
 [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53],
 [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59],
 [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61]
  ...

-} 