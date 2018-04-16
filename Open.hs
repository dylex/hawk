{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Open
  ( hawkOpen
  ) where

import           Control.Monad (unless, forM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import           Database.PostgreSQL.Typed (pgConnect, pgDisconnect)
import           System.FilePath (takeExtension)

import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit2 as WK

import Types
import Config
import Cookies
import Bind

setStyle :: Gtk.IsWidget w => w -> BS.ByteString -> IO Gtk.CssProvider
setStyle obj rules = do
  css <- Gtk.cssProviderNew
  style <- Gtk.widgetGetStyleContext obj
  #addProvider style css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
  #loadFromData css rules
  return css

toHex :: Double -> BSB.Builder
toHex x
  | x <= 0 = "00"
  | x >= 1 = "ff"
  | otherwise = BSB.word8HexFixed $ floor (255*x)
  where

hawkOpen :: Config -> IO Hawk
hawkOpen hawkConfig@Config{..} = do
  hawkDatabase <- mapM pgConnect configDatabase

  hawkWindow <- G.new Gtk.Window
    [ #type G.:= Gtk.WindowTypeToplevel
    ]

  hawkTopBox <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical ]
  #add hawkWindow hawkTopBox

  hawkStatusBox <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal ]
  hawkStatusStyle <- setStyle hawkStatusBox "*{}"
  #packEnd hawkTopBox hawkStatusBox False False 0

  -- status left
  hawkStatusCount <- G.new Gtk.Label []
  _ <- setStyle hawkStatusCount "*{background-color:#8cc;color:#000;}"
  #packStart hawkStatusBox hawkStatusCount False False 0

  hawkStatusLeft <- G.new Gtk.Label
    [ #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #setMarkup hawkStatusLeft "Loading..."
  #packStart hawkStatusBox hawkStatusLeft False False 0

  -- status right
  hawkStatusLoad <- G.new Gtk.Label []
  _ <- setStyle hawkStatusLoad "*{color:#fff;}"
  hawkStatusLoadStyle <- setStyle hawkStatusLoad "*{}"
  #packEnd hawkStatusBox hawkStatusLoad False False 0

  hawkStatusURI <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignEnd
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #packEnd hawkStatusBox hawkStatusURI True True 0


  hawkSettings <- WK.settingsNew
  unless (V.null configUserAgent) $
    G.set hawkSettings [#userAgent G.:= V.unsafeHead configUserAgent]
  forM_ (HM.toList configSettings) $ \(k, v) ->
    setObjectProperty hawkSettings k v

  hawkStyleSheets <- forM configStyleSheet $ \f -> do
    d <- TIO.readFile f
    WK.userStyleSheetNew d WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing

  hawkScripts <- forM configScript $ \f -> do
    d <- TIO.readFile f
    WK.userScriptNew d WK.UserContentInjectedFramesAllFrames WK.UserScriptInjectionTimeStart Nothing Nothing

  hawkUserContentManager <- WK.userContentManagerNew
  unless (V.null hawkStyleSheets) $
    #addStyleSheet hawkUserContentManager $ V.unsafeHead hawkStyleSheets
  unless (V.null hawkScripts) $
    #addScript hawkUserContentManager $ V.unsafeHead hawkScripts

  hawkWebsiteDataManager <- maybe
    WK.websiteDataManagerNewEphemeral
    (\d -> G.new WK.WebsiteDataManager
      [ #baseDataDirectory G.:= T.pack d ])
    configDataDirectory
  hawkCookieManager <- #getCookieManager hawkWebsiteDataManager
  {- This doesn't seem to work:
  forM_ configCookieFile $ \f ->
    #setPersistentStorage hawkCookieManager (T.pack f) (case takeExtension f of
      ".txt" -> WK.CookiePersistentStorageText
      "" -> WK.CookiePersistentStorageText
      _ -> WK.CookiePersistentStorageSqlite) -}
  #setAcceptPolicy hawkCookieManager configCookieAcceptPolicy

  hawkWebContext <- WK.webContextNewWithWebsiteDataManager hawkWebsiteDataManager
  #setCacheModel hawkWebContext configCacheModel
  #setWebProcessCountLimit hawkWebContext configProcessCountLimit
  {- haskell-gi #154
  forM_ configProxy $ \p ->
    #setNetworkProxySettings hawkWebContext WK.NetworkProxyModeCustom . Just
      =<< WK.networkProxySettingsNew (Just p) configProxyIgnore
  -}
  #setSpellCheckingEnabled hawkWebContext configSpellChecking
  #setProcessModel hawkWebContext configProcessModel

  hawkWebView <- G.new WK.WebView
    [ #webContext G.:= hawkWebContext
    , #editable G.:= configEditable
    , #userContentManager G.:= hawkUserContentManager
    , #zoomLevel G.:= configZoomLevel
    ]
  #setCustomCharset hawkWebView configCharset
  #packStart hawkTopBox hawkWebView True True 0


  _ <- G.on hawkWebView #loadChanged $ \ev ->
    Gtk.labelSetText hawkStatusLoad $ case ev of
      WK.LoadEventStarted -> "WAIT"
      WK.LoadEventRedirected -> "REDIR"
      WK.LoadEventCommitted -> "RECV"
      WK.LoadEventFinished -> ""
      (WK.AnotherLoadEvent x) -> T.pack $ show x

  _ <- G.on hawkWebView (G.PropertyNotify #estimatedLoadProgress) $ \_ -> do
    p <- G.get hawkWebView #estimatedLoadProgress
    #loadFromData hawkStatusLoadStyle $ BSL.toStrict $ BSB.toLazyByteString $ "*{background-color:#" <> toHex (1-p) <> toHex p <> "00;}"

  _ <- G.on hawkWebView (G.PropertyNotify #uri) $ \_ ->
    Gtk.labelSetText hawkStatusURI . fold =<< G.get hawkWebView #uri

  hawkBindings <- newIORef def
  hawkStyleSheet <- newIORef 0
  hawkPrivateMode <- newIORef configPrivateMode

  let hawk = Hawk{..}

  _ <- G.after hawkWebView #close $ #destroy hawkWindow
  mapM_ (G.after hawkWindow #destroy . pgDisconnect) hawkDatabase
  _ <- G.on hawkWindow #keyPressEvent $ runHawkM hawk . runBind

  runHawkM hawk loadCookies

  #showAll hawkWindow

  mapM_ (#loadUri hawkWebView) configURI
  return hawk
