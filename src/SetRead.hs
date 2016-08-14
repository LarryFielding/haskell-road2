instance (Eq a, Read a) => Read (Set a) where 
   readsPrec _ = 
           readParen False     (\r -> [pr | ("{",s) <- lex r,
                                            pr      <- readl s ])
           where readl  s =  [(St [],t)   | ("}",t) <- lex s] ++
                      [(insertSet x xs,u) | (x,t)   <- reads s,
                                            (xs,u)  <- readl' t]
                 readl' s =  [(St [],t)   | ("}",t) <- lex s] ++
                      [(insertSet x xs,v) | (",",t) <- lex s,
                                            (x,u)   <- reads t,
                                            (xs,v)  <- readl' u]
