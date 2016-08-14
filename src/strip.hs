main = do fromHandle <- getAndOpenFile "Copy from" ReadMode
          toHandle <- getAndOpen