module Util
  ( mintersperse
  , justIf
  , foldMapA
  , fromDoesNotExist
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard)
import           Data.Foldable (find)
import           Data.Functor.Identity (Identity(..))
import           Data.Monoid ((<>))
import           System.IO.Error (isDoesNotExistError)

mintersperse :: Monoid m => m -> [m] -> m
mintersperse _ [] = mempty
mintersperse d (x:l) = x <> mconcat (map (d <>) l)

justIf :: (a -> Bool) -> a -> Maybe a
justIf f = find f . Identity

foldMapA :: (Applicative f, Monoid m, Foldable t) => (a -> f m) -> t a -> f m
foldMapA f = foldr (\a r -> mappend <$> f a <*> r) (pure mempty)

fromDoesNotExist :: a -> IO a -> IO a
fromDoesNotExist d = handleJust (guard . isDoesNotExistError) (const $ return d)
