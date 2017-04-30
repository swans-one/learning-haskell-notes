import Data.Functor.Classes (Eq1(..), Show1(..))

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1. Write the Functor instance for EitherT
instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . (fmap . fmap $ f) . runEitherT


-- 2. Write the Applicative instance for EitherT
instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  f <*> a = EitherT $ (fmap (<*>) (runEitherT f)) <*> runEitherT a


-- 3. Write the Monad instance for EitherT
instance Monad m => Monad (EitherT e m) where
  return = pure
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  v >>= f = EitherT $ do
    ea <- runEitherT v
    case fmap f ea of
      (Left e) -> pure (Left e)
      (Right x) -> runEitherT x

-- f :: a -> EitherT e m a
-- (runEitherT v) :: m (Either e a)
-- (runEitherT . f) :: a -> m (Either e a)

-- 4. Write the swapEither helper function for EitherT
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT et = EitherT $ fmap swapEither (runEitherT et)
  where swapEither (Left x) = (Right x)
        swapEither (Right x) = (Left x)

-- 5. Write the transformer variant of the either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb et = (runEitherT et) >>= f
  where f (Left a) = fa a
        f (Right b) = fb b


-- Testing Support and Tests
instance (Show e, Show a, Show1 m) => Show (EitherT e m a) where
  show = ("EitherT " ++) . show1m . runEitherT
    where show1m x = liftShowsPrec showsPrec showList 0 x $ ""

instance
  (Arbitrary e, Monad m, Arbitrary a)
  => Arbitrary (EitherT e m a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements
      [ (EitherT . return . Right) a
      , (EitherT . return . Left) e ]

instance (Eq e, Eq a, Eq1 m) => EqProp (EitherT e m a) where
  a =-= b = property $ a == b

instance (Eq e, Eq1 m) => Eq1 (EitherT e m) where
  liftEq f x y = liftEq (liftEq f) (runEitherT x) (runEitherT y)

instance (Eq e, Eq a, Eq1 m) => Eq (EitherT e m a) where
  a == b = liftEq (==) (runEitherT a) (runEitherT b)

printArbitraryEitherT :: IO ()
printArbitraryEitherT = do
  a <- generate (arbitrary :: Gen (EitherT String Maybe Int))
  putStrLn (show . runEitherT $ a)

main :: IO ()
main = hspec $ do
  describe "functor instance" $ do
    it "passes checkers" $ do
      let trigger = (undefined :: (EitherT String Maybe (Int, Int, Int)))
      quickBatch $ functor trigger

  describe "applicative instance" $ do
    it "passes checkers" $ do
      let trigger = (undefined :: (EitherT String Maybe (Int, Int, Int)))
      quickBatch $ applicative trigger

  describe "monad instance" $ do
    it "passes checkers" $ do
      let trigger = (undefined :: (EitherT String Maybe (Int, Int, Int)))
      quickBatch $ monad trigger

    it "is usable" $ do
      let a = EitherT [Left "err", Right (3 :: Int)]
      let f = (\x -> EitherT [Right (x + 1), Right (x * 3)])
      (a >>= f) `shouldBe` EitherT [Left "err", Right 4, Right 9]

  describe "swapEitherT" $ do
    it "swaps elements of list" $ do
      let input = EitherT [Left "err", Right 3]
      let expected = EitherT [Right "err", Left 3]
      swapEitherT input `shouldBe` expected

  describe "eitherT" $ do
    it "maps left and right" $ do
      let a = EitherT . Just . Left $ "error"
      let b = EitherT . Just . Right $ 4
      let fa = Just . length
      let fb = Just . (+1)
      eitherT fa fb a `shouldBe` eitherT fa fb b
