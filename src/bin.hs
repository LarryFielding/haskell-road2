binary :: Int -> [Int]
binary x = reverse (bin x)
  where bin 0 = [0]
        bin 1 = [1]
        bin n = (rem n 2): bin (quot n 2)