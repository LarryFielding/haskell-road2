-- see John Stillwell, Mathematics and Its History, Ch 3 

f 0 = (1,0) 
f (n+1) = (a + 2*b, a + b) where a = fst (f n) 
                                 b = snd (f n) 

ints2rat :: (Integer,Integer) -> Rational
ints2rat (n,m) = n % m

op :: (Integer,Integer) -> (Integer,Integer)
op (x,y) = (x + 2*y, x + y) 

fs = (1,0) : map op fs

sqrt2 = map ints2rat (tail fs)

sq2 :: [Float]
sq2 = map fromRational sqrt2


