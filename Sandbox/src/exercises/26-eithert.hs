import Data.Functor.Classes (Eq1(..), Show1(..))

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1. Write the Functor instance for EitherT
instance Functor m => Functor (EitherT e m) where
  fmap = undefined


-- 2. Write the Applicative instance for EitherT
instance Applicative m => Applicative (EitherT e m) where
  pure = undefined
  f <*> a = undefined


-- 3. Write the Monad instance for EitherT
instance Monad m => Monad (EitherT e m) where
  return = pure
  v >>= f = undefined


-- 4. Write the swapEither helper function for EitherT
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = undefined

-- 5. Write the transformer variant of the either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT = undefined


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
      , (EitherT . return . Left) e
      ]

instance (Eq e, Eq a, Eq1 m) => EqProp (EitherT e m a) where
  a =-= b = property $ a == b

instance (Eq e, Eq1 m) => Eq1 (EitherT e m) where
  liftEq f x y = liftEq eqf (runEitherT x) (runEitherT y)
    where eqf (Left x) (Left y) = x == y
          eqf (Right x) (Right y) = f x y
          eqf _ _ = False

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
