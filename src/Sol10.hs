module Sol10

where 

import COR

evens = 0 : map (+2) evens

theEvens = iterate (+2) 0

swap ""        = ""
swap ('1': xs) = '0': swap xs
swap ('0': xs) = '1': swap xs

morse xs = xs ++ morse (xs ++ swap xs)

thue = '0' : morse "1"

random001s :: Int -> [Int]
random001s i =  map (`mod` 2) (randomInts 2 i)

vend, vend1, vend2, vend3, vend4  :: Process
vend  (0:xs) = "coin"     : vend1 xs
vend  (1:xs) = "coin"     : vend4 xs
vend1 (0:xs) = "coin"     : vend2 xs
vend1 (1:xs) =              vend1 xs
vend2 (0:xs) = "beer"     : vend xs
vend2 (1:xs) = "coin"     : vend3 xs
vend3 (0:xs) = "moneyback": vend  xs
vend3 (1:xs) =              vend3 xs
vend4 (0:xs) = "water"    : vend xs
vend4 (1:xs) =              vend4 xs

