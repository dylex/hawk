module UI
  ( setStyle
  , hawkClose
  , hawkGoto
  , setStatusLeft
  , loadStyleSheet
  , passThruBind
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified GI.Gtk as Gtk

import Types
import Expand

setStyle :: (Gtk.IsWidget w, MonadIO m) => w -> BS.ByteString -> m Gtk.CssProvider
setStyle obj rules = do
  css <- Gtk.cssProviderNew
  style <- Gtk.widgetGetStyleContext obj
  #addProvider style css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
  #loadFromData css rules
  return css

hawkClose :: HawkM ()
hawkClose = do
  win <- asks hawkWindow
  #destroy win

hawkGoto :: T.Text -> HawkM ()
hawkGoto url = do
  wv <- askWebView
  #loadUri wv =<< expandURI url

setStatusLeft :: T.Text -> HawkM ()
setStatusLeft t = do
  stat <- asks hawkStatusLeft
  #setText stat t

loadStyleSheet :: Int -> HawkM ()
loadStyleSheet i = do
  cm <- askUserContentManager
  glob <- asksGlobal globalStyleSheet
  css <- asks hawkStyleSheets
  #removeAllStyleSheets cm
  #addStyleSheet cm glob
  mapM_ (#addStyleSheet cm) $ css V.!? i
  writeRef hawkStyleSheet i

passThruBind :: HawkM ()
passThruBind = do
  css <- asks hawkStatusStyle
  #loadFromData css "*{background-color:#000;}"
  modifyRef_ hawkBindings $ \bind ->
    case bind of
      PassThru{} -> bind
      _ -> PassThru $ do
        #loadFromData css "*{}"
        return bind
