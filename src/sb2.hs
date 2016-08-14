{-- Generation of the Stern Brocot tree of rational numbers --} 
{-- See Concrete Mathematics, Section 4.5                   --}

sb :: [[Rational]]
sb = genFractionTree [(0 % 1)]

genFractionTree :: [Rational] -> [[Rational]]
genFractionTree fractions = 
 fractions : genFractionTree (newfractions fractions)
 where 
 newfractions [x] = [x, ((numerator x + 1) % (denominator x))]
 newfractions (x : y : fractions) = 
     x : between : newfractions (y : fractions)
  where 
  between = (((numerator x) + (numerator y))
                % ((denominator x) + (denominator y)))
               