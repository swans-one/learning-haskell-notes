import Test.QuickCheck
import Test.Hspec
import qualified Data.Monoid as Mo

class Semigroup s where
  (<>) :: s -> s -> s

-- 1: Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2: Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdStringAssoc =
  Identity String -> Identity String -> Identity String -> Bool

-- 3: Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoStringAssoc =
  Two String String -> Two String String -> Two String String -> Bool

type TwoStringListIntAssoc =
  Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

-- 4 Three

data Three a b c = Three a b c

-- This should be just like Two, but longer. Skipping for now.

-- 5 Four

data Four a b c d = Four a b c d

-- This should be just like Two and Three but longer. Skipping for now.

-- 6 BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = oneof [return $ BoolConj True, return $ BoolConj False]

-- 7 BoolDisj

newtype BoolDisj = BoolDisj Bool

-- This should be very similar to BoolConj. Skipping for now

-- 8 Or

data Or a b = Fst a | Snd b

-- instance Semigroup (Or a b) where
--   (Or)


-- 9 Combine a b

-- Combine is a representation of a unary function
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  f <> g = Combine $ \x -> (f' x) <> (g' x)
    where f' = unCombine f
          g' = unCombine g

-- main: hSpec & quickcheck

-- This rule will be reused for each of our tests
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- These classes are for testing

instance Semigroup [a] where
  x <> y = x ++ y

instance Num a => Semigroup (Mo.Sum a) where
  (Mo.Sum x) <> (Mo.Sum y) = Mo.Sum $ x + y

main :: IO ()
main = hspec $ do
  describe "Trivial" $ do
    it "'s semigroup is associative" $ do
      quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)

  describe "Identity" $ do
    it "'s semigroup is associative" $ do
      quickCheck (semigroupAssoc :: IdStringAssoc)

  describe "Two" $ do
    it "'s semigroup is associative with two String" $ do
      quickCheck (semigroupAssoc :: TwoStringAssoc)
    it "'s semigroup is associative with a String and an [Int]" $ do
      quickCheck (semigroupAssoc :: TwoStringListIntAssoc)

  describe "BoolConj" $ do
    it "'s semigroup is associative" $ do
      quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)

  describe "Combine" $ do
    it "works at all" $ do
      (unCombine $ Combine (+ 1)) 1 `shouldBe` 2
    it "'s semigroup does things we expect" $ do
      let f = Combine $ \n -> Mo.Sum (n + 1)
      let g = Combine $ \n -> Mo.Sum (n - 1)

      (unCombine (f <> g) $ 0) `shouldBe` 0
      -- (unCombine (f <> g) $ 1) `shouldBe` 2
      -- (unCombine (f <> f) $ 1) `shouldBe` 4
      -- (unCombine (g <> f) $ 1) `shouldBe` 2
      1 `shouldBe` 1

    -- it "'s semigroup is associative" $ do
    --   -- Ooops, I failed to be able to do quickcheck with coarbitrary
    --   1 `shouldBe` 0
