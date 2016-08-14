geoProgression :: Rational -> Rational -> [Rational]
geoProgression a r = [ a * r^n | n <- [0..] ]

mechanicsRule :: Rational -> Rational -> Rational 
mechanicsRule p x = (1 % 2) * (x + (p * (recip x)))

mechanics :: Rational -> Rational -> [Rational]
mechanics p x = iterate (mechanicsRule p) x

sqrtApprox :: Rational -> [Rational]
sqrtApprox p | p < 0     = error "negative argument"
             | otherwise = mechanics p s
  where s = max 1 (pred (until (\ m -> m^2 > p) succ 1))


