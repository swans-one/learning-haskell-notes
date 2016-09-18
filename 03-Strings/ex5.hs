rvrs :: String -> String
rvrs s = concat [awe, " ", is, " ", curry]
  where curry = take 5 s
        is    = take 2 $ drop 6 s
        awe   = drop 9 s
