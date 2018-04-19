module Util
  ( mintersperse
  , foldMapA
  , catchDoesNotExist
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard)
import           Data.Monoid ((<>))
import           System.IO.Error (isDoesNotExistError)

-- not really json-specific
mintersperse :: Monoid m => m -> [m] -> m
mintersperse _ [] = mempty
mintersperse d (x:l) = x <> mconcat (map (d <>) l)

foldMapA :: (Applicative f, Monoid m, Foldable t) => (a -> f m) -> t a -> f m
foldMapA f = foldr (\a r -> mappend <$> f a <*> r) (pure mempty)

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist = handleJust (guard . isDoesNotExistError) (const $ return Nothing) . fmap Just
