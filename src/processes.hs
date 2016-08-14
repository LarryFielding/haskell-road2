import Random (mkStdGen,randomRs)

randomInts :: Int -> Int -> [Int]
randomInts i j =  tail (randomRs (0,i) (mkStdGen j))

random001s :: Int -> [Int]
random001s i =  map (`mod` 2) (randomInts 2 i) 

type Process = [Int] -> [String]

start :: Process -> Int -> Int -> [String]
start process i j = process (randomInts i j)

-- clock 

clock :: Process 
clock (0:xs) = "tick" : clock xs
clock (1:xs) = "crack" : []

clock1 :: Process 
clock1 (0:xs) = "tick" : clock2 xs
clock1 (1:xs) = "crack" : []

clock2 :: Process 
clock2 (0:xs) = "tick" : clock1 xs
clock2 (1:xs) = "crack" : []

-- vending machines 

vending, vending1, vending2 :: Process
vending  (0:xs) = "coin"     : vending1 xs
vending  (1:xs) =              vending  xs
vending1 (0:xs) = "coin"     : vending2 xs
vending1 (1:xs) = "water"    : vending  xs 
vending2 (0:xs) = "coin"     : vending3 xs
vending2 (1:xs) = "beer"     : vending  xs
vending3 (0:xs) = "moneyback": vending  xs
vending3 (1:xs) =              vending3 xs

vend,vend1,vend2,vend3 :: Process 
vend  (0:xs) = "coin"      : vend1 xs
vend  (1:xs) = "coin"      : vend4 xs
vend1 (0:xs) = "coin"      : vend2 xs
vend1 (1:xs) =               vend1 xs
vend2 (0:xs) = "coin"      : vend  xs
vend2 (1:xs) = "beer"      : vend  xs
vend3 (0:xs) = "moneyback" : vend  xs
vend3 (1:xs) =               vend3 xs
vend4 (0:xs) = "coin"      : vend  xs
vend4 (1:xs) =               vend4 xs


-- parking ticket dispenser 

ptd :: Process
ptd = ptd0 0 

ptd0 :: Int -> Process
ptd0 0 (0:xs) = ptd0 0 xs
ptd0 i (0:xs) = ("return " ++ show i ++ " euro") : ptd0 0 xs
ptd0 i (1:xs) = "1 euro" : ptd0 (i+1) xs
ptd0 i (2:xs) = "2 euro" : ptd0 (i+2) xs
ptd0 0 (3:xs) = ptd0 0 xs
ptd0 i (3:xs) = ("ticket " ++ show (i * 20) ++ " min") : ptd0 0 xs


-- interacting processes 

actions   = user [0,0,1] responses
responses = vending actions 

user acts ~(r:s:p:resps) = acts ++ user (proc [r,s,p]) resps

proc ["coin","coin","beer"] =  [0,0,1]


