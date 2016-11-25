data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i >= 0    = Just (mkNat i)
  | otherwise = Nothing
  where mkNat 0 = Zero
        mkNat x = Succ (mkNat $ x - 1)
