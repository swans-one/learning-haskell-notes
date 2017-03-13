newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi newG
    where newG s = ((f . fst . g) s, (snd . g) s)

instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- (Moi f) <*> (Moi g) = Moi (\x -> (((fst $ f x) . fst . g $ x), (snd . g $ x)))

  (Moi f) <*> (Moi g) = Moi applied
    where applied s =
            let (f', s') = f s
                (val, state) = g s'
            in (f' val, state)

main :: IO ()
main = do
  print $ runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
