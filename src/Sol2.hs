module Sol2  where 

import GS
import TAMO
-- use hugs -98 
-- or use ghci -XFlexibleInstances

tst1a = not True <=> False
tst1b = not False <=> True
tst2  = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
tst3a = logEquiv1 (\ p -> p || True) (const True)
tst3b = logEquiv1 (\ p -> p && False) (const False)
tst4a = logEquiv1 (\ p -> p || False) id
tst4b = logEquiv1 (\ p -> p && True) id 
tst5  = logEquiv1 excluded_middle (const True)
tst6  = logEquiv1 (\ p -> p && not p) (const False)

contrad1 :: (Bool -> Bool) -> Bool
contrad1 bf =  not (bf True) && not (bf False)

contrad2 :: (Bool -> Bool -> Bool)  -> Bool
contrad2 bf = and [not (bf p q) | p <- [True,False], q <- [True,False]]

contrad3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contrad3 bf = and [ not (bf p q r) | p <- [True,False], 
                                     q <- [True,False], 
                                     r <- [True,False]] 

unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

parity :: [Bool] -> Bool
parity [] = True
parity (x:xs) = x /= (parity xs)

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p

