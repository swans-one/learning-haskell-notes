module Cipher where

import Data.Char

newtype Plaintext  = Plain  [Char] deriving (Show, Eq)
newtype Ciphertext = Cipher [Char] deriving (Show, Eq)
newtype Keyword    = Key    [Char] deriving (Show, Eq)


encode :: Keyword -> Plaintext -> Ciphertext
encode (Key key) (Plain plain) =
  Cipher (map rotateOne (zipMatchSpaces (repeat' key) plain))
  where rotateOne (k, p) = rotateChar True k p

decode :: Keyword -> Ciphertext -> Plaintext
decode (Key key) (Cipher cipher) =
  Plain (map rotateOne (zipMatchSpaces (repeat' key) cipher))
  where rotateOne (k, p) = rotateChar False k p


rotateChar :: Bool -> Char -> Char -> Char
rotateChar forward rot char
  | 'a' <= char && char <= 'z' =
    rotateByBase 'a' (if forward then rot else invFromBase 'a' rot) char
  | 'A' <= char && char <= 'Z' =
    rotateByBase 'A' (if forward then rot else invFromBase 'A' rot) char
  | ' ' == char = ' '
  | otherwise = '!'

rotateByBase :: Char -> Char -> Char -> Char
rotateByBase base rot char =
  inCharRange addAndRot base char
  where addAndRot = (flip mod 26) . (+ (ord rot - ord base))


invFromBase :: Char -> Char -> Char
invFromBase = inCharRange ((flip mod 26) . ((-) 26))

inCharRange :: (Int -> Int) -> Char -> Char -> Char
inCharRange f base char = doUndo (subBase . ord) (chr . addBase) f char
  where addBase   = (+ (ord base))
        subBase   = (flip (-) (ord base))

-- Do a function inside a transformation and its inverse
doUndo :: (a -> b) -> (b -> a) -> (b -> b) -> a -> a
doUndo f inv g = inv . g . f

repeat' :: [a] -> [a]
repeat' xs = xs ++ (repeat' xs)

zipMatchSpaces :: [Char] -> [Char] -> [(Char, Char)]
zipMatchSpaces [] _ = []
zipMatchSpaces _ [] = []
zipMatchSpaces (x:xs) (y:ys)
  | x == ' ' = (' ', ' ') : (zipMatchSpaces xs (y:ys))
  | y == ' ' = (' ', ' ') : (zipMatchSpaces (x:xs) ys)
  | otherwise = (x, y) : (zipMatchSpaces xs ys)

-- A note on how I went about this originally. Concise, but hard to
-- read, and required and integer argument.
--
-- rotateByBase i c base =
--   chr . (+ base_i) . (flip mod 26) . (+ i) . (flip (-) base_i) $ c_i
--   where base_i = ord base
--         c_i    = ord c
