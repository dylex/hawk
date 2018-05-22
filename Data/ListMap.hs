{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Data.ListMap
  ( ListMap
  , empty
  , null
  , singleton
  , insert
  , insertWith
  , delete
  , alter
  , union
  , unionWith
  , lookup
  , lookupSubtree
  , lookupPrefix
  , lookupPrefixes
  , lookupFoldPrefixes
  , toList
  , fromList
  ) where

import Prelude hiding (lookup, null, filter)

import           Control.Applicative (Alternative, (<|>))
import qualified Control.Applicative as A
import           Control.Arrow (first)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import           Data.Foldable (fold)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as M
import           Data.Monoid ((<>))

import Util

type Key k = (Eq k, Hashable k)

-- |A map in which each key is a list (a trie)
data ListMap k a = ListMap
  { _listMapValue :: !(Maybe a)
  , _listMap :: !(HM.HashMap k (ListMap k a))
  }
  deriving (Show)

instance Functor (ListMap k) where
  fmap f (ListMap v x) = ListMap (fmap f v) (fmap (fmap f) x)

instance Key k => Semigroup (ListMap k a) where
  (<>) = union

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

-- |Insert a new entry to the map, using the given function to merge with any existing key.
insertWith :: Key k => (a -> a -> a) -> [k] -> a -> ListMap k a -> ListMap k a
insertWith f k v = alter (Just . maybe v (f v)) k

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

-- |Union two maps, left-biased.
union :: Key k => ListMap k a -> ListMap k a -> ListMap k a
union (ListMap v1 m1) (ListMap v2 m2) = ListMap (v1 <|> v2) (M.unionWith union m1 m2)

-- |Lookup an exact key in the map.
lookup :: Key k => [k] -> ListMap k a -> Maybe a
lookup [] (ListMap v _) = v
lookup (n:d) (ListMap _ m) = M.lookup n m >>= lookup d

-- |Lookup an exact key in the map and return it and its subtree.
lookupSubtree :: Key k => [k] -> ListMap k a -> Maybe (ListMap k a)
lookupSubtree [] m = Just m
lookupSubtree (n:d) (ListMap _ m) = M.lookup n m >>= lookupSubtree d

-- |Lookup a key or any prefix of that key (special case of 'lookupPrefixes').
lookupPrefix :: Key k => [k] -> ListMap k a -> Maybe a
lookupPrefix [] (ListMap v _) = v
lookupPrefix (n:d) (ListMap v m) = (M.lookup n m >>= lookupPrefix d) <|> v

-- |Lookup all prefixes of a key, in most-to-least specific order.
lookupPrefixes :: (Key k, Alternative f) => [k] -> ListMap k a -> f a
lookupPrefixes l (ListMap v m) = lp l <|> maybe A.empty pure v where
  lp [] = A.empty
  lp (n:d) = maybe A.empty (lookupPrefixes d) $ M.lookup n m

-- |Lookup and fold all prefixes of a key, in most-to-least specific order.
lookupFoldPrefixes :: (Key k, Monoid a) => [k] -> ListMap k a -> a
lookupFoldPrefixes l (ListMap v m) = lp l <> fold v where
  lp [] = mempty
  lp (n:d) = foldMap (lookupFoldPrefixes d) $ M.lookup n m

toList :: ListMap k a -> [([k],a)]
toList (ListMap v m) = maybe id ((:) . ([] ,)) v $ foldMap tal $ M.toList m where
  tal (n,d) = map (first (n:)) $ toList d

fromList :: (Foldable f, Key k) => f ([k],a) -> ListMap k a
fromList = foldMap $ uncurry singleton

-- |Expanded list mapping (not the inverse of ToJSON)
instance (J.FromJSONKey k, J.FromJSON k, Key k) => J.FromJSON1 (ListMap k) where
  liftParseJSON par parl j@(J.Object _) =
    fromList . HM.toList <$> J.liftParseJSON par parl j
  liftParseJSON par _ j = singleton [] <$> par j

instance (J.FromJSONKey k, J.FromJSON k, Key k, J.FromJSON a) => J.FromJSON (ListMap k a) where
  parseJSON = J.parseJSON1

-- |Direct nested representation as {$:val,map...}
instance J.ToJSONKey k => J.ToJSON1 (ListMap k) where
  liftToJSON to _ = toj where
    toj (ListMap v m) = J.Object
      $ maybe id (HM.insert "$" . to) v
      $ M.foldrWithKey (\k -> HM.insert (tok k) . toj) HM.empty m
    J.ToJSONKeyText tok _ = J.toJSONKey
  liftToEncoding to _ = toe where
    toe (ListMap v m) = JE.pairs
      $ foldMap (JE.pair "$" . to) v
      <> M.foldrWithKey (\k -> mappend . JE.pair' (tok k) . toe) mempty m
    J.ToJSONKeyText _ tok = J.toJSONKey
    
instance (J.ToJSONKey k, J.ToJSON a) => J.ToJSON (ListMap k a) where
  toJSON = J.toJSON1
  toEncoding = J.toEncoding1
