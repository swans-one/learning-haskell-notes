module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this

myLines :: String -> [String]
myLines s = go s []
  where go s acc
          | s /= ""   = go (drop 1 rest) (acc ++ [first])
          | otherwise = acc
          where first = takeWhile (/= '\n') s
                rest  = dropWhile (/= '\n') s

-- What we want 'myLines sentences' to equal
shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]
