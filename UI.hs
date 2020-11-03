{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module UI
  ( setStyle
  , pasteSelection
  , copySelection
  , hawkClose
  , hawkGoto
  , setStatusLeft
  , loadStyleSheet
  , modifyCount
  , commandModeBind
  , passThruBind
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32)

import qualified GI.Gdk as Gdk
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

pasteSelection :: (T.Text -> HawkM ()) -> HawkM ()
pasteSelection f = do
  hawk <- ask
  #requestText (hawkClipboard $ hawkGlobal hawk) $ \_ -> maybe (return ()) $ runHawkM hawk . f

copySelection :: T.Text -> HawkM ()
copySelection t = do
  sel <- asksGlobal hawkClipboard
  Gtk.clipboardSetText sel t (fromIntegral $ T.length t)

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
  css <- asksGlobal hawkStyleSheets
  #removeAllStyleSheets cm
  #addStyleSheet cm glob
  mapM_ (#addStyleSheet cm) $ css V.!? i
  writeRef hawkStyleSheet i

modifyCount :: (Maybe Word32 -> Maybe Word32) -> HawkM (Maybe Word32)
modifyCount f = do
  c <- modifyRef hawkBindings $ \case
    b@Command{ commandCount = c } -> (b{ commandCount = f c }, c)
    b -> (b, Nothing)
  stat <- asks hawkStatusCount
  #setText stat $ maybe T.empty (T.pack . show) $ f c
  return c

commandModeBind :: HawkM ()
commandModeBind = do
  unpass =<< readRef hawkBindings
  _ <- modifyCount (const Nothing)
  setStatusLeft T.empty
  #searchFinish =<< askFindController
  -- writeRef hawkBindings def
  where
  unpass (PassThru Gdk.KEY_Escape r) = unpass =<< r
  unpass _ = return ()

passThruBind :: Word32 -> HawkM ()
passThruBind k = do
  css <- asks hawkStatusStyle
  #loadFromData css "*{background-color:#000;}"
  modifyRef_ hawkBindings $ \case
    bind@PassThru{} -> bind{ passThruKey = k }
    bind -> PassThru k $ do
      #loadFromData css "*{}"
      return bind
