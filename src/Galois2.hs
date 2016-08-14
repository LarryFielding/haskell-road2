module Galois2

where 

data Galois2 = Zero | One deriving (Eq, Ord, Enum, Read, Bounded)

instance Show Galois2 
  where show Zero = "0"
        show One  = "1"

instance Num Galois2 
  where 
  Zero + One    = One 
  One  + Zero   = One 
  _    + _      = Zero 
  One  * One    = One 
  _    * _      = Zero
  negate        = id
  abs           = id
  signum        = id 
  fromInteger n | even n    = Zero
                | otherwise = One 



  