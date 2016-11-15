{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

-- Question 1

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

out1 = tooMany ((55 :: Int), "hello")

newtype LG =
  LG (Int, String) deriving (Eq, Show)

instance TooMany LG where
  tooMany (LG (n, _)) = n > 42

out2 = tooMany (LG (55, "hello"))

-- Question 2

instance TooMany (Int, Int) where
  tooMany (f1, f2) = (f1 + f2) > 42

-- Question 3

instance TooMany Int where
  tooMany n = n > 42

newtype TwoTooMany (Num a, TooMany a) => (a, a) =
  TTM (a, a) deriving (Eq, Show)

instance TooMany TwoTwooMany where
  tooMany (x, y) = x + y > 42
