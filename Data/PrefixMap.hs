{-# LANGUAGE ConstraintKinds #-}

module Data.PrefixMap
  ( PrefixMap
  , empty
  , null
  , singleton
  , insert
  , delete
  , alter
  , union
  , lookup
  , lookupPrefix
  , filter
  , foldrTree
  , foldTree
  , toList
  , fromList
  ) where

import Prelude hiding (lookup, null, filter)

import           Control.Arrow (first)
import           Data.Foldable (fold)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import           Data.Monoid ((<>))

import Util

type Key k = (Eq k, Hashable k)

-- |A map in which each key is a list and no key is a prefix of any other key.
data PrefixMap k a
  = Leaf !a
  | Node !(M.HashMap k (PrefixMap k a))
  deriving (Show)

instance Functor (PrefixMap k) where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node m) = Node $ fmap (fmap f) m

instance Key k => Semigroup (PrefixMap k a) where
  (<>) = union

instance Key k => Monoid (PrefixMap k a) where
  mempty = empty
  mappend = union

-- this may only appear at the top
empty :: PrefixMap k a
empty = Node M.empty

null :: PrefixMap k a -> Bool
null (Node m) = M.null m
null _ = False

nonull :: PrefixMap k a -> Maybe (PrefixMap k a)
nonull = justIf (not . null)

singleton :: Key k => [k] -> a -> PrefixMap k a
singleton [] = Leaf
singleton (n:d) = Node . M.singleton n . singleton d

-- |Insert a new entry to the map.  Any existing conflicting entries (prefixes or elongations of the given key) are replaced.
insert :: Key k => [k] -> a -> PrefixMap k a -> PrefixMap k a
insert [] x _ = Leaf x
insert d x (Leaf _) = singleton d x
insert (n:d) x (Node m) = Node $ M.insertWith (\_ -> insert d x) n (singleton d x) m

-- |Remove a single entry from the map, if present.
delete :: Key k => [k] -> PrefixMap k a -> PrefixMap k a
delete [] (Leaf _) = empty
delete (n:d) (Node m) = Node $ M.update (nonull . delete d) n m
delete _ d = d

-- |Modify, insert, or delete the value associated with a key.  Note that when the supplied update function is 'id' the map is not modified in any way and specifically conflicting entries are preserved.
alter :: Key k => (Maybe a -> Maybe a) -> [k] -> PrefixMap k a -> PrefixMap k a
alter f [] (Leaf x)
  | Just y <- f (Just x) = Leaf y
  | otherwise = empty
alter f (n:k) (Node m) = Node $ M.alter (nonull . alter f k . fold) n m
alter f k t
  | Just y <- f Nothing = singleton k y
  | otherwise = t -- empty?

-- |Union two maps; more general values (shorter keys) always take precedence.
union :: Key k => PrefixMap k a -> PrefixMap k a -> PrefixMap k a
union l@(Leaf _) _ = l
union _ l@(Leaf _) = l
union (Node a) (Node b) = Node $ M.unionWith union a b

-- |Lookup an exact key in the map.
lookup :: Key k => [k] -> PrefixMap k a -> Maybe a
lookup [] (Leaf x) = Just x
lookup (n:d) (Node m) = M.lookup n m >>= lookup d
lookup _ _ = Nothing

-- |Lookup a key or any prefix of that key.
lookupPrefix :: Key k => [k] -> PrefixMap k a -> Maybe a
lookupPrefix _ (Leaf x) = Just x
lookupPrefix (n:d) (Node m) = M.lookup n m >>= lookupPrefix d
lookupPrefix _ _ = Nothing

filter :: Key k => (a -> Bool) -> PrefixMap k a -> PrefixMap k a
filter p (Leaf a)
  | p a = Leaf a
  | otherwise = empty
filter p (Node m) = Node $ M.mapMaybe (nonull . filter p) m where

foldrTree :: (k -> b -> b -> b) -> (a -> b) -> b -> PrefixMap k a -> b
foldrTree _ fl _ (Leaf x) = fl x
foldrTree fn fl b (Node m) = M.foldrWithKey (\n -> fn n . foldrTree fn fl b) b m

foldTree :: Monoid a => (k -> a -> a) -> PrefixMap k a -> a
foldTree f = foldrTree (\k a b -> f k a <> b) id mempty

toList :: PrefixMap k a -> [([k],a)]
toList (Leaf x) = [([],x)]
toList (Node m) = foldMap tal $ M.toList m where
  tal (n,d) = map (first (n:)) $ toList d

fromList :: (Foldable f, Key k) => f ([k],a) -> PrefixMap k a
fromList = foldMap $ uncurry singleton
