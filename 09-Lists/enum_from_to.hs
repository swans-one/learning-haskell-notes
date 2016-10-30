eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool x _ = [x, x]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd start end
  | start == end = [start, end]
  | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt start end = go start end []
  where go start end acc
           | start <= end = go start (pred end) (end : acc)
           | otherwise    = acc

eftChar :: Char -> Char -> [Char]
eftChar start end = go start end []
  where go start end acc
           | start <= end = go start (pred end) (end : acc)
           | otherwise    = acc
