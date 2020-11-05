{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MonoidList
  ( MonoidList(..)
  ) where

newtype MonoidList a = MonoidList{ monoidList :: [a] }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

instance Semigroup a => Semigroup (MonoidList a) where
  MonoidList [] <> x = x
  x <> MonoidList [] = x
  MonoidList (a:x) <> MonoidList (b:y) = MonoidList (a <> b : monoidList (MonoidList x <> MonoidList y))

instance Semigroup a => Monoid (MonoidList a) where
  mempty = MonoidList []
