import Data.Monoid

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two i g) (Two x y) = Two (mappend i x) (g y)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y


-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three x y z) = Three (a <> x) (b <> y) (f z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f g) (Three' x y z) = Three' (a <> x) (f y) (g z)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

-- I don't find this more interesting than Three

-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

-- I don't find this more interesting than Three'


main :: IO ()
main = hspec $ do

  describe "Pair" $ do
    it "passes functor laws" $ do
      quickBatch $ functor (undefined :: Pair (Int, Int, Int))
    it "passes applicative laws" $ do
      quickBatch $ applicative (undefined :: Pair (Int, Int, Int))

  describe "Two" $ do
    it "passes functor laws" $ do
      quickBatch $ functor (undefined :: Two String (Int, Int, Int))
    it "passes applicative laws" $ do
      quickBatch $ applicative (undefined :: Two String (Int, Int, Int))
    it "passes a simple interchange" $ do
      let u = Two "hi" (+2)
      let lhs = u <*> pure 1
      let rhs = pure ($ 1) <*> u
      lhs `shouldBe` rhs

  describe "Three" $ do
    it "passes functor laws" $ do
      quickBatch $ functor (undefined :: Three String String (Int, Int, Int))
    it "passes applicative laws" $ do
      quickBatch $ applicative (undefined :: Three String String (Int, Int, Int))

  describe "Three'" $ do
    it "passes functor laws" $ do
      quickBatch $ functor (undefined :: Three' String (Int, Int, Int))
    it "passes applicative laws" $ do
      quickBatch $ applicative (undefined :: Three' String (Int, Int, Int))
