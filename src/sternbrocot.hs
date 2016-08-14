{-- Generation of the Stern Brocot tree of rational numbers --} 
{-- See Concrete Mathematics, Section 4.5                   --}

type Fraction = (Integer,Integer)

sb :: [[Fraction]]
sb = genFractionTree [(0,1),(1,0)]

genFractionTree :: [Fraction] -> [[Fraction]]
genFractionTree fractions = 
 fractions : genFractionTree (newfractions fractions)
 where 
 newfractions [x] = [x]
 newfractions ((m,n) : (m',n') : fractions) = 
     (m,n) : (m+m',n+n') : newfractions ((m',n') : fractions)
               