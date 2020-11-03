module Content
  ( addContentFilter
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import qualified GI.GLib as GLib

import Types

addContentFilter :: J.ToJSON f => T.Text -> f -> HawkM ()
addContentFilter i j = do
  cm <- askUserContentManager
  fs <- asksGlobal hawkFilterStore
  b <- GLib.bytesNew $ Just $ BSL.toStrict $ J.encode j
  #save fs i b noCancellable $ Just $ \_ r -> do
    f <- #saveFinish fs r
    #addFilter cm f
