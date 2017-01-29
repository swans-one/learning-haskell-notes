import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--
-- 1) The Nope Monad where nothing happens and nobody cares.
--

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq


--
-- 2) MyEither
--

data MyEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (MyEither b) where
  fmap _ (Right' b) = (Right' b)
  fmap f (Left' a) = Left' $ f a

instance Applicative (MyEither b) where
  pure x = Left' x
  (Left' f) <*> (Left' x) = Left' $ f x
  (Right' x) <*> _ = Right' x
  _ <*> (Right' x) = Right' x

instance Monad (MyEither b) where
  return = pure
  Left' x >>= f = f x
  Right' x >>= _ = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Left' a), (1, return $ Right' b)]

instance (Eq a, Eq b) =>EqProp (MyEither a b) where (=-=) = eq


--
-- 3) Write a monad instance for identity
--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

--
-- 4) This should be easier than writing the applicative instance:
--

data List' a = Nil | Cons a (List' a) deriving (Eq, Show)

instance Functor List' where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List' where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs xs = flatMap (<$> xs) fs

instance Monad List' where
  return x = Cons x Nil
  Nil >>= f = Nil
  Cons x y >>= f = (f x) `append` (y >>= f)

append :: List' a -> List' a -> List' a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List' a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List' (List' a) -> List' a
concat' = fold append Nil

flatMap :: (a -> List' b) -> List' a -> List' b
flatMap f as = concat' $ fmap f as

listOfLists = (Cons
                (Cons 1 (Cons 2 (Cons 3 Nil)))
                (Cons (Cons 2 (Cons 4 (Cons 6 Nil))) Nil))

instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = do
    elem <- arbitrary
    nextList <- arbitrary
    frequency [(1, return Nil), (10, return (Cons elem nextList))]

instance Eq a => EqProp (List' a) where (=-=) = eq


main :: IO ()
main = do
  hspec $ do

    describe "Nope" $ do
      it "passes the laws" $ do
        let trigger = (undefined :: Nope (Int, String, Int))
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger

    describe "MyEither" $ do
      it "passes the laws" $ do
        let trigger = (undefined :: MyEither String (Int, String, Int))
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger

    describe "Identity" $ do
      it "passes the laws" $ do
        let trigger = (undefined :: Identity (Int, String, Int))
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger

    describe "List'" $ do
      it "passes the laws" $ do
        let trigger = (undefined :: List' (Int, String, Int))
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
