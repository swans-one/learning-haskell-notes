listify :: [Char] -> [[Char]]
listify s = go s []
  where go s acc
           | s /= ""  = go (drop 1 rest) (acc ++ [first])
           | otherwise   = acc
           where first = takeWhile (/= ' ') s
                 rest  = dropWhile (/= ' ') s
