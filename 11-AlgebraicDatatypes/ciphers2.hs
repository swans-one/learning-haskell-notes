module Cipher2 where

import Data.Char

data Mod26Char =
    Mod26Char Int
  | SpaceC
  | OtherC
  deriving (Show, Eq)

newtype Keyword = Key [Char] deriving (Show, Eq)


encode :: Keyword -> [Char] -> [Char]
encode (Key key) plain =
  map (toChar . (uncurry addMod26)) (ziptoMod26 plain (repeat' key))

decode :: Keyword -> [Char] -> [Char]
decode (Key key) cipher =
  map (toChar . (uncurry subMod26)) (ziptoMod26 cipher (repeat' key))

ziptoMod26 :: [Char] -> [Char] -> [(Mod26Char, Mod26Char)]
ziptoMod26 x y = zipMatchSpaces (map toMod26 x) (map toMod26 y)

zipMatchSpaces :: [Mod26Char] -> [Mod26Char] -> [(Mod26Char, Mod26Char)]
zipMatchSpaces [] _ = []
zipMatchSpaces _ [] = []
zipMatchSpaces (x:xs) (y:ys)
  | x == SpaceC = (SpaceC, SpaceC) : (zipMatchSpaces xs (y:ys))
  | y == SpaceC = (SpaceC, SpaceC) : (zipMatchSpaces (x:xs) ys)
  | otherwise = (x, y) : (zipMatchSpaces xs ys)

toMod26 :: Char -> Mod26Char
toMod26 char
  | 'a' <= char && char <= 'z' = Mod26Char (ord char - ord 'a')
  | 'A' <= char && char <= 'Z' = Mod26Char (ord char - ord 'A')
  | char == ' ' = SpaceC
  | otherwise = OtherC

toChar :: Mod26Char -> Char
toChar (Mod26Char i) = chr (i + (ord 'A'))
toChar SpaceC = ' '
toChar OtherC = '!'

addMod26 :: Mod26Char -> Mod26Char -> Mod26Char
addMod26 (Mod26Char x) (Mod26Char y) = Mod26Char (mod (x + y) 26)
addMod26 x y
  | x == SpaceC || y == SpaceC = SpaceC
  | x == OtherC || y == OtherC = OtherC

subMod26 :: Mod26Char -> Mod26Char -> Mod26Char
subMod26 (Mod26Char x) (Mod26Char y) = Mod26Char (mod (x - y) 26)
subMod26 x y
  | x == SpaceC || y == SpaceC = SpaceC
  | x == OtherC || y == OtherC = OtherC

repeat' :: [a] -> [a]
repeat' xs = xs ++ (repeat' xs)

mapBoth :: (a -> b) -> [(a, a)] -> [(b, b)]
mapBoth f xs = map (\ (x, y) -> (f x, f y)) xs
