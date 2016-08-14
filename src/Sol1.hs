module Sol1  where 

import GS

mxmInt :: [Int] -> Int
mxmInt [] = error "empty list" 
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

removeFst :: Eq a => a -> [a] -> [a] 
removeFst x [] = []
removeFst x (y:ys) | x == y    = ys
                   | otherwise = y : (removeFst x ys)

count :: Char -> String -> Int 
count c [] = 0 
count c (x:xs) | c==x      = 1 + (count c xs)
               | otherwise = (count c xs)

copy :: Int -> Char -> String
copy 0 c = []
copy n c = c:(copy (n-1) c)

blowup :: String -> String 
blowup xs = blowup' xs 1

blowup' :: String -> Int -> String 
blowup' [] n = []
blowup' (x:xs) n = (copy n x) ++ (blowup' xs (n+1))

spread :: [a] -> [a]
spread xs = [ x | (n,y) <- zip [1..] xs , x <- take n (repeat y)]

mnm :: Ord a => [a] -> a
mnm [] = error "empty list" 
mnm [x] = x
mnm (x:xs) = min x (mnm xs)

srt :: Ord a => [a] -> [a] 
srt [] = []
srt xs = m : (srt (removeFst m xs)) where m = mnm xs 

substring :: String -> String -> Bool
substring [] ys = True
substring (x:xs) [] = False
substring (x:xs) (y:ys) = ((x==y) && (prefix xs ys)) 
                        || (substring (x:xs) ys) 

lengths :: [[a]] -> [Int]
lengths = map length

sumLengths :: [[a]] -> Int 
sumLengths = sum . lengths

