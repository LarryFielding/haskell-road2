type Fraction = Ratio Integer

geoSeries :: Fraction -> Fraction -> [Fraction]
geoSeries a r = [ a * r^n | n <- [0..] ]
