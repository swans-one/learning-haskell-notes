{-# LANGUAGE FlexibleInstances #-}

module FunctorImplementations where

import Test.Hspec


-- 1: Quant

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk  a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2: K

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K x) = K x

-- 3 (requires the flexible instances declaration):

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype J a b = J a deriving (Eq, Show)

instance Functor (Flip J a) where
  fmap f (Flip (J x)) = (Flip (J (f x)))

-- 4:

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5: Do we need somethinge extra to make the instance work?

data LiftItOut f a = LiftItOut (f a)

-- instance Functor LiftItOut where

-- 6:

data Parappa f g a = DaWrappa (f a) (g a)

-- 7 Don't ask for more typeclass instances than you need:

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)

-- 8:

data Notorious g o a t = Notorious (g o) (g a) (g t)

-- 9 You'll need to use recursion:

data List a = Nil | Cons a (List a)

-- 10:

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

-- 11 Use an extra functor fo this. Do your best:

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)


-- Testing utils

plusp = (++ "p")

main :: IO ()
main = do
  hspec $ do

    describe "1 Quant" $ do
      it "Works for Finance" $ do
        (fmap (+1) Finance) `shouldBe` (Finance :: Quant Int Int)
      it "Works for Bloor" $ do
        let res = fmap (+1) (Bloor 2)
        res `shouldBe` (Bloor 3 :: Quant Int Int)
      it "works for Desk" $ do
        let res = (fmap (+1) $ Desk "hello") :: Quant String Int
        let expected = (Desk "hello") :: Quant String Int
        res `shouldBe` expected

    describe "2 K" $ do
      it "Works for something" $ do
        (fmap plusp (K "hello")) `shouldBe` (K "hello")

    describe "3 Flip J" $ do
      it "Works for something" $ do
        (fmap plusp (Flip (J "hello"))) `shouldBe` (Flip (J "hellop"))


    describe "4 EvilGoateeConst" $ do
      it "Works for something" $ do
        (fmap plusp (GoatyConst "hello")) `shouldBe` (GoatyConst "hellop")

    describe "5" $ do
      it "Works for something" $ do
        True

    describe "6" $ do
      it "Works for something" $ do
        True

    describe "7" $ do
      it "Works for something" $ do
        True

    describe "8" $ do
      it "Works for something" $ do
        True

    describe "9" $ do
      it "Works for something" $ do
        True

    describe "10" $ do
      it "Works for something" $ do
        True

    describe "11" $ do
      it "Works for something" $ do
        True
