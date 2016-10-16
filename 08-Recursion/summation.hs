decTowardsZero :: (Num a, Ord a) => a -> a
decTowardsZero n
  | n < 0 = n + 1
  | n > 0 = n - 1

summation :: (Eq a, Num a) => a -> a
summation n = go n 0
  where go n count
         | n == 0 = count
         | otherwise = go (n - 1) (count + n)

summation' :: (Ord a, Num a) => a -> a
summation' n = go n 0
  where go n count
         | n == 0 = count
         | otherwise = go (decTowardsZero n) (count + n)
