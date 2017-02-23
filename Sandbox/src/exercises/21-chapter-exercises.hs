import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary


-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]


-- List

-- Three

-- Three'

-- S

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S xs y) = S (f <$> xs) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S xs y) = mappend (foldMap f xs) (f y)

instance Traversable n => Traversable (S n) where
  traverse f (S xs y) = S <$> (traverse f xs) <*> (f y)

-- f = flip lookup [(1, 'a'), (2, 'b')]
-- S [1, 2, 3] 2
-- Prelude> traverse f $ S [1, 2] 2
-- Just S "ab" 'b'
-- Prelude> traverse f $ S [1, 2, 3], 2
-- Nothing


-- Tree

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l c r) = Node (fmap f l) (f c) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l c r) = foldMap f l `mappend` f c `mappend`  foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l c r) = Node <$> (traverse f l) <*> (f c) <*> (traverse f r)

main = do
  let id_trig = undefined :: Identity (Int, Int, [Int])
  putStrLn "\nIdentity:"
  quickBatch (traversable id_trig)

  let opt_trig = undefined :: Optional (Int, Int, [Int])
  putStrLn "\nOption:"
  quickBatch (traversable opt_trig)

  let const_trig = undefined :: Constant Int (Int, Int, [Int])
  putStrLn "\nConstant:"
  quickBatch (traversable const_trig)

  -- let s_trig = undefined :: S (Int, Int, [Int]) Int
  -- quickBatch (traversable s_trig)
