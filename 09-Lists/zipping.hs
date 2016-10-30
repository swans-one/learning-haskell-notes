module Zipping where

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys


myZipWhere :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWhere f [] _ = []
myZipWhere f _ [] = []
myZipWhere f (x:xs) (y:ys) = f x y : myZipWhere f xs ys

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWhere (\x y -> (x, y))
