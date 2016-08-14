module Nico where 

import Polynomials
import PowerSeries

nico = map fromEnum "Bye Nico! " 

nicoForever = nico ++ nicoForever

bn = 66 + 121*z  + 101*z^2 + 32*z^3 + 78*z^4 + 105*z^5 + 99*z^6 + 111*z^7 + 33*z^8 + 32*z^9

g = (66 + 121*z  + 101*z^2 + 32*z^3 + 78*z^4 + 105*z^5 + 99*z^6 + 111*z^7 + 33*z^8 + 32*z^9) / (1-z^10)

bye = take 1000 (map (toEnum . round) g :: [Char])
