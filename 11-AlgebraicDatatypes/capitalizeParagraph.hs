module CapPar where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord (c:cs) = (toUpper c):cs

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . (capStartWords True) . words
    where capStartWords _ [] = []
          capStartWords True (x:xs)
            | last x == '.' = (capitalizeWord x):(capStartWords True xs)
            | otherwise     = (capitalizeWord x):(capStartWords False xs)
          capStartWords False (x:xs)
            | last x == '.' = x:(capStartWords True xs)
            | otherwise     = x:(capStartWords False xs)
