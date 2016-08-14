solveQdr :: (Float,Float,Float) -> (Float,Float) 
solveQdr (a,b,c) = let d = b^2 - 4*a*c in 
                   if d < 0 then error "no real solutions"
                   else 
                     ((- b + sqrt d) / 2*a,
                      (- b - sqrt d) / 2*a)

solveQdr1 :: (Float,Float,Float) -> (Float,Float) 
solveQdr1 =  \ (a,b,c) -> let d = b^2 - 4*a*c in 
                          if d < 0 then error "no real solutions"
                          else 
                           ((- b + sqrt d) / 2*a,
                            (- b - sqrt d) / 2*a)


--((- b + sqrt (b^2 - 4*a*c)) / 2*a, 
--                 (- b - sqrt (b^2 - 4*a*c)) / 2*a)
