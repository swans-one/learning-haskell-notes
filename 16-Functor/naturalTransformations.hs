{-# LANGUAGE RankNTypes #-}

module NaturalTransformations where

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- The following won't typecheck, because we can't actually know
-- anything about the a.
--
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]
