theLucs = 2 : 1 : zipWith (+) theLucs (tail theLucs)

f n = theLucs !! (n) * (theLucs !! (n+2)) - (theLucs !! (n+1))^2
