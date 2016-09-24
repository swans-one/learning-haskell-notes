module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

-- Look at the type for this. How does it compare to f?
f' x y = x + y + 3
