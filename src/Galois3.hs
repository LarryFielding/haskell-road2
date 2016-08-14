module Galois3

where 

data Galois3 = G0 | G1 | G2 deriving (Eq, Ord, Enum, Read, Bounded)

instance Show Galois3 
  where show G0 = "0"
        show G1 = "1"
        show G2 = "2"

instance Num Galois3 
  where 
  g1 + g2    = toEnum ((fromEnum g1 + fromEnum g2) `mod` 3)
  g1 * g2    = toEnum ((fromEnum g1 * fromEnum g2) `mod` 3)
  negate G0  = G0
  negate G1  = G2
  negate G2  = G1
  abs        = id
  signum G0  = G0
  signum _   = G1
  fromInteger n | n `rem` 3 == 0 = G0
                | n `rem` 3 == 1 = G1
                | n `rem` 3 == 2 = G2



  