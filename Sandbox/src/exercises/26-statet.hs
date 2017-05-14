
-- This is Jesse's implemntation. I think it breaks the applicative
-- laws.
instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \x -> pure (a, s)
  (StateT smf) <*> (StateT sma) =
    StateT $ \s -> (flip (,) s <$>)
                   $ (fst (<$>) smf s)
                   <*> (fst <$> sma s)
