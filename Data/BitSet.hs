{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BitSet
  ( BitSet
  , empty
  , full
  , null
  , isFull
  , size
  , singleton
  , member
  , findMin
  , findMax
  , insert
  , delete
  , union
  , intersection
  , difference
  , fromList
  , toList
  , foldr
  , foldl
  , foldMap
  ) where

import Prelude hiding (null, foldMap, foldr, foldl)

import           Control.Applicative ((<|>))
import qualified Data.Aeson as J
import           Data.Bits (Bits, bit, testBit, setBit, clearBit, complement, (.|.), (.&.), popCount, FiniteBits, finiteBitSize, countLeadingZeros, countTrailingZeros)
import           Data.List (foldl')
import qualified Data.Vector as V

newtype BitSet a = BitSet Word
  deriving (Eq, Bits, FiniteBits, J.ToJSON, Show)

instance Semigroup (BitSet a) where
  (<>) = union

instance Monoid (BitSet a) where
  mempty = empty
  mappend = union

empty :: BitSet a
empty = BitSet 0

null :: BitSet a -> Bool
null (BitSet 0) = True
null _ = False

size :: BitSet a -> Int
size = popCount

full :: forall a . (Enum a, Bounded a) => BitSet a
full = BitSet $ pred $ bit $ succ $ fromEnum (maxBound :: a)

isFull :: forall a . (Enum a, Bounded a) => BitSet a -> Bool
isFull = (succ (fromEnum (maxBound :: a)) ==) . size

singleton :: Enum a => a -> BitSet a
singleton = bit . fromEnum

member :: Enum a => a -> BitSet a -> Bool
member a x = testBit x $ fromEnum a

findMax :: Enum a => BitSet a -> a
findMax x = toEnum $ pred $ finiteBitSize x - countLeadingZeros x

findMin :: Enum a => BitSet a -> a
findMin x = toEnum $ countTrailingZeros x

insert :: Enum a => a -> BitSet a -> BitSet a
insert a x = setBit x $ fromEnum a

delete :: Enum a => a -> BitSet a -> BitSet a
delete a x = clearBit x $ fromEnum a

union :: BitSet a -> BitSet a -> BitSet a
union = (.|.)

intersection :: BitSet a -> BitSet a -> BitSet a
intersection = (.&.)

difference :: BitSet a -> BitSet a -> BitSet a
difference a b = a .&. complement b

fromList :: Enum a => Foldable f => f a -> BitSet a
fromList = foldl' (flip insert) empty

toList :: Enum a => BitSet a -> [a]
toList (BitSet 0) = []
toList x = toEnum i : toList (clearBit x i) where
  i = countTrailingZeros x

foldMap :: (Enum a, Monoid m) => (a -> m) -> BitSet a -> m
foldMap _ (BitSet 0) = mempty
foldMap f x = f (toEnum i) <> foldMap f (clearBit x i) where
  i = countTrailingZeros x

foldr :: Enum a => (a -> b -> b) -> b -> BitSet a -> b
foldr _ b (BitSet 0) = b
foldr f b x = f (toEnum i) $ foldr f b (clearBit x i) where
  i = countTrailingZeros x

foldl :: Enum a => (b -> a -> b) -> b -> BitSet a -> b
foldl _ b (BitSet 0) = b
foldl f b x = f (foldl f b (clearBit x i)) (toEnum i) where
  i = pred $ finiteBitSize x - countLeadingZeros x

instance (J.FromJSON a, Enum a, Bounded a) => J.FromJSON (BitSet a) where
  parseJSON J.Null = return empty
  parseJSON (J.Bool False) = return empty
  parseJSON (J.Bool True)  = return full
  parseJSON (J.Array v) = fromList <$> V.mapM J.parseJSON v
  parseJSON v = BitSet <$> J.parseJSON v <|> singleton <$> J.parseJSON v
