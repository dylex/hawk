{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module URI.ListMap
  ( ListMap
  , empty
  , null
  , singleton
  , insert
  , delete
  , alter
  , unionWith
  , lookup
  , lookupPrefix
  , toList
  , fromList
  ) where

import Prelude hiding (lookup, null, filter)

import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import           Data.Foldable (fold)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M

import Util

type Key k = (Eq k, Hashable k)

-- |A map in which each key is a list (a trie)
data ListMap k a = ListMap
  { _listMapValue :: !(Maybe a)
  , _listMap :: !(M.HashMap k (ListMap k a))
  }
  deriving (Show)

instance Functor (ListMap k) where
  fmap f (ListMap v x) = ListMap (fmap f v) (fmap (fmap f) x)

instance Key k => Monoid (ListMap k a) where
  mempty = empty
  mappend = union

-- this may only appear at the top
empty :: ListMap k a
empty = ListMap Nothing M.empty

null :: ListMap k a -> Bool
null (ListMap Nothing m) = M.null m
null _ = False

nonull :: ListMap k a -> Maybe (ListMap k a)
nonull = justIf (not . null)

leaf :: a -> ListMap k a
leaf v = ListMap (Just v) M.empty

singleton :: Key k => [k] -> a -> ListMap k a
singleton [] = leaf
singleton (n:d) = ListMap Nothing . M.singleton n . singleton d

alter :: Key k => (Maybe a -> Maybe a) -> [k] -> ListMap k a -> ListMap k a
alter f [] (ListMap v m) = ListMap (f v) m
alter f (n:d) (ListMap v m) = ListMap v $ M.alter (nonull . alter f d . fold) n m

-- |Insert a new entry to the map.  Any existing entry are replaced.
insert :: Key k => [k] -> a -> ListMap k a -> ListMap k a
insert k v = alter (const $ Just v) k

-- |Remove a single entry from the map, if present.
delete :: Key k => [k] -> ListMap k a -> ListMap k a
delete = alter (const Nothing)

unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith _ Nothing x = x
unionMaybeWith _ x Nothing = x
unionMaybeWith f (Just x) (Just y) = Just $ f x y

-- |Union two maps
unionWith :: Key k => (a -> a -> a) -> ListMap k a -> ListMap k a -> ListMap k a
unionWith f (ListMap v1 m1) (ListMap v2 m2) = ListMap (unionMaybeWith f v1 v2) (M.unionWith (unionWith f) m1 m2)

-- |Union two maps; more general values (shorter keys) always take precedence.
union :: Key k => ListMap k a -> ListMap k a -> ListMap k a
union (ListMap v1 m1) (ListMap v2 m2) = ListMap (v1 <|> v2) (M.unionWith union m1 m2)

-- |Lookup an exact key in the map.
lookup :: Key k => [k] -> ListMap k a -> Maybe a
lookup [] (ListMap v _) = v
lookup (n:d) (ListMap _ m) = M.lookup n m >>= lookup d

-- |Lookup a key or any prefix of that key.
lookupPrefix :: Key k => [k] -> ListMap k a -> Maybe a
lookupPrefix [] (ListMap v _) = v
lookupPrefix (n:d) (ListMap v m) = (M.lookup n m >>= lookupPrefix d) <|> v

toList :: ListMap k a -> [([k],a)]
toList (ListMap v m) = maybe id ((:) . ([] ,)) v $ foldMap tal $ M.toList m where
  tal (n,d) = map (first (n:)) $ toList d

fromList :: (Foldable f, Key k) => f ([k],a) -> ListMap k a
fromList = foldMap $ uncurry singleton
