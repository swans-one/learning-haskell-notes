module ListApplicativeExercies where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List' a = Nil | Cons a (List' a) deriving (Eq, Show)

instance Functor List' where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List' where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs xs = flatMap (<$> xs) fs

instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = do
    elem <- arbitrary
    nextList <- arbitrary
    frequency [(1, return Nil), (10, return (Cons elem nextList))]

instance Eq a => EqProp (List' a) where (=-=) = eq

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

main :: IO ()
main = hspec $ do
  describe "applicative <*>" $ do
    it "basically works" $ do
      let result = (Cons (+1) (Cons (+2) Nil)) <*> (Cons 1 (Cons 2 Nil))
      let expected = (Cons 2 (Cons 3 (Cons 3 (Cons 4 Nil))))
      result `shouldBe` expected
    it "quick checks" $ do
      quickBatch $ applicative (undefined :: List' (String, String, Int))
