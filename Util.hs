module Util
  ( mwhen
  , mintersperse
  , mintersperseMap
  , justIf
  , foldMapA
  , app
  , fromDoesNotExist
  , makeLenses'
  ) where

import           Control.Exception (handleJust)
import qualified Control.Lens as Lens
import           Control.Monad (guard)
import           Data.Foldable (find)
import           Data.Functor.Identity (Identity(..))
import           Data.Monoid ((<>))
import qualified Language.Haskell.TH as TH
import           System.IO.Error (isDoesNotExistError)

mwhen :: Monoid m => Bool -> m -> m
mwhen True v = v
mwhen False _ = mempty

mintersperse :: Monoid m => m -> [m] -> m
mintersperse d = mintersperseMap d id

mintersperseMap :: Monoid m => m -> (a -> m) -> [a] -> m
mintersperseMap _ _ [] = mempty
mintersperseMap d f (x:l) = f x <> mconcat (map ((<>) d . f) l)

justIf :: (a -> Bool) -> a -> Maybe a
justIf f = find f . Identity

foldMapA :: (Applicative f, Monoid m, Foldable t) => (a -> f m) -> t a -> f m
foldMapA f = foldr (\a r -> mappend <$> f a <*> r) (pure mempty)

app :: Applicative f => f (a -> b) -> a -> f b
app f = (<*>) f . pure

fromDoesNotExist :: a -> IO a -> IO a
fromDoesNotExist d = handleJust (guard . isDoesNotExistError) (const $ return d)

makeLenses' :: TH.Name -> TH.DecsQ
makeLenses' = Lens.makeLensesWith $ Lens.lensField Lens..~ Lens.mappingNamer (return . (++ "'")) $ Lens.lensRules
