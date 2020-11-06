{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.EnumVec
  ( EnumVec
  , fromList
  , toList
  , assocList
  , enumVec
  , insert
  , lookup
  , empty
  , efoldr
  , unionWith
  , union
  , lens
  ) where

import Prelude hiding (lookup)

import qualified Control.Lens as Lens
import qualified Data.Aeson as J
import           Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import qualified Data.Vector as V

newtype EnumVec e v = EnumVec { enumVector :: V.Vector v }
  deriving (Eq, Ord, Show, J.ToJSON)
type EnumIx e = (Enum e, Bounded e)

instance Semigroup v => Semigroup (EnumVec e v) where
  (<>) = union

instance (EnumIx e, Monoid v) => Monoid (EnumVec e v) where
  mempty = empty

ix :: EnumIx e => e -> Int
ix e = fromEnum e -- - fromEnum (minBound `asTypeOf` e)

len :: EnumIx e => Proxy e -> Int
len p = succ $ ix (maxBound `asProxyTypeOf` p)

fromList :: forall e v . EnumIx e => [v] -> EnumVec e v
fromList = EnumVec . V.fromListN (len (Proxy :: Proxy e))

toList :: EnumVec e v -> [v]
toList = V.toList . enumVector

efoldr :: EnumIx e => (e -> v -> a -> a) -> a -> EnumVec e v -> a
efoldr f a = V.ifoldr (f . toEnum) a . enumVector

assocList :: forall e v . EnumIx e => EnumVec e v -> [(e,v)]
assocList = 
  V.ifoldr (\i -> (:) . (toEnum i, )) [] . enumVector
  -- zip (enumFromTo (minBound :: e) (maxBound :: e)) . toList

enumVec :: forall e v . EnumIx e => v -> EnumVec e v
enumVec = EnumVec . V.replicate (len (Proxy :: Proxy e))

lookup :: EnumIx e => e -> EnumVec e v -> v
lookup e v = enumVector v V.! ix e

insert :: EnumIx e => e -> v -> EnumVec e v -> EnumVec e v
insert e v = EnumVec . (V.// [(ix e, v)]) . enumVector

unionWith :: (v -> v -> v) -> EnumVec e v -> EnumVec e v -> EnumVec e v
unionWith f a b = EnumVec $ V.zipWith f (enumVector a) (enumVector b)

empty :: EnumIx e => Monoid v => EnumVec e v
empty = enumVec mempty

union :: Semigroup v => EnumVec e v -> EnumVec e v -> EnumVec e v
union = unionWith (<>)

lens :: EnumIx e => e -> Lens.Lens' (EnumVec e v) v
lens e = Lens.lens (lookup e) (flip $ insert e)
