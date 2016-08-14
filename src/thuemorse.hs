-----------------------------------------------------------------
--                                                             --
--  Corecursive definition of                                  --
--                                                             --
--    Thue-Morse sequences                                     --
--                                                             --
--                                                             --
--  Jan van Eijck, 28 juni 2002                                --
--                                                             --
--                                                             --
-----------------------------------------------------------------

-- Corecursive way: 

swap ""        = ""
swap ('1': xs) = '0': swap xs
swap ('0': xs) = '1': swap xs

morse xs = swap xs ++ morse (xs ++ swap xs)

thue = '0' : morse "0"

-- `Traditional' recursive way: 

expand ""       = ""
expand ('0':xs) = '0':'1': expand xs
expand ('1':xs) = '1':'0': expand xs 

thuemorse 0 = "0"
thuemorse (n+1) = expand (thuemorse n)

