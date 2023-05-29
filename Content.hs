module Content
  ( addContentFilter
  , updateFilters
  , resetFilters
  ) where

import           Control.Monad.Trans.Reader (ask)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import qualified GI.GLib as GLib

import Types
import Config
import ContentFilter (ContentRule)
import Filter

addContentFilter :: J.ToJSON f => T.Text -> f -> HawkM () -> HawkM ()
addContentFilter i j callback = do
  cm <- askUserContentManager
  fs <- asksGlobal hawkFilterStore
  b <- GLib.bytesNew $ Just $ BSL.toStrict $ J.encode j
  hawk <- ask
  #save fs i b noCancellable $ Just $ \_ r _ -> do
    f <- #saveFinish fs r
    #addFilter cm f
    runHawkM hawk callback

filterID :: T.Text
filterID = "filter"

setFilters :: [ContentRule] -> HawkM () -> HawkM ()
setFilters = addContentFilter filterID

updateFilters :: HawkM () -> HawkM ()
updateFilters callback = do
  f <- readRef hawkFilters
  let r = filterRules f
  setFilters r callback

resetFilters :: HawkM () -> HawkM ()
resetFilters callback = do
  writeRef hawkFilters =<< asksConfig configFilters
  updateFilters callback
