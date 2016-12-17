module InstancesOfFunc where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

-- 1:
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 2:
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

-- 3:
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- 4:
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

-- 5:
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

-- 6:
data Four' a b = Four' a b c d deriving (Eq, Show)

-- 7:
data Four' a b = Four' a a a b deriving (Eq, Show)

-- 8: Can you even implement one for this type? Why or why not?

data Trivial = Trivial

-- You cannot. Functor requires a higher kinded type. Specifically a
-- type of kind ~* -> *~. Trivial is of kind ~*~.

-- Extra QuickCheck Business

functorIdentity :: (Eq (f c), Functor f) => f c -> Bool
functorIdentity functor = fmap id functor == functor

functorCompose :: (Eq (f c), Functor f) =>
                  f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  hspec $ do

    describe "1: Identity a" $ do
      it "passes identity" $ do
        quickCheck (functorIdentity :: (Identity Char) -> Bool)
      it "passes composition" $ do
        quickCheck (functorCompose ::
                       (Identity Int) -> IntToInt -> IntToInt -> Bool)

    describe "2: Pair a" $ do
      it "passes identity" $ do
        quickCheck (functorIdentity :: (Pair Int) -> Bool)
      it "passes composition" $ do
        quickCheck (functorCompose ::
                       (Pair Int) -> IntToInt -> IntToInt -> Bool)

    describe "3: Two a b" $ do
      it "passes identity" $ do
        quickCheck (functorIdentity :: (Two Int Char) -> Bool)
      it "passes composition" $ do
        quickCheck (functorCompose ::
                       (Two Char Int) -> IntToInt -> IntToInt -> Bool)

    describe "4: Three a b c" $ do
      it "passes identity" $ do
        quickCheck (functorIdentity :: (Three Int Char Float) -> Bool)
      it "passes composition" $ do
        quickCheck (functorCompose ::
                       (Three Char Float Int) -> IntToInt -> IntToInt -> Bool)

    describe "5: Three' a b b" $ do
      it "passes identity" $ do
        quickCheck (functorIdentity :: (Three' Int Char) -> Bool)
      it "passes composition" $ do
        quickCheck (functorCompose ::
                       (Three' Char Int) -> IntToInt -> IntToInt -> Bool)
