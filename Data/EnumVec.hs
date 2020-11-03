{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.EnumVec
  ( EnumVec
  , fromList
  , toList
  , assocList
  , enumVec
  , set
  , get
  , empty
  , unionWith
  , union
  , lens
  ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as J
import           Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import qualified Data.Vector as V

newtype EnumVec e v = EnumVec { enumVector :: V.Vector v }
  deriving (J.ToJSON)
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

assocList :: forall e v . EnumIx e => EnumVec e v -> [(e,v)]
assocList = zip (enumFromTo (minBound :: e) (maxBound :: e)) . toList

enumVec :: forall e v . EnumIx e => v -> EnumVec e v
enumVec = EnumVec . V.replicate (len (Proxy :: Proxy e))

get :: EnumIx e => EnumVec e v -> e -> v
get v e = enumVector v V.! ix e

set :: EnumIx e => e -> v -> EnumVec e v -> EnumVec e v
set e v = EnumVec . (V.// [(ix e, v)]) . enumVector

unionWith :: (v -> v -> v) -> EnumVec e v -> EnumVec e v -> EnumVec e v
unionWith f a b = EnumVec $ V.zipWith f (enumVector a) (enumVector b)

empty :: EnumIx e => Monoid v => EnumVec e v
empty = enumVec mempty

union :: Semigroup v => EnumVec e v -> EnumVec e v -> EnumVec e v
union = unionWith (<>)

lens :: EnumIx e => e -> Lens.Lens' (EnumVec e v) v
lens e = Lens.lens (flip get e) (flip $ set e)
