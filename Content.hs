module Content
  ( addContentFilter
  , updateFilters
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Yaml as Y

import qualified GI.GLib as GLib

import Types
import Filter

addContentFilter :: J.ToJSON f => T.Text -> f -> HawkM ()
addContentFilter i j = do
  cm <- askUserContentManager
  fs <- asksGlobal hawkFilterStore
  b <- GLib.bytesNew $ Just $ BSL.toStrict $ J.encode j
  #save fs i b noCancellable $ Just $ \_ r -> do
    f <- #saveFinish fs r
    #addFilter cm f

updateFilters :: HawkM ()
updateFilters = do
  f <- readRef hawkFilters
  let r = filterRules f
  liftIO $ BSC.putStrLn $ Y.encode r
  addContentFilter "filter" r
