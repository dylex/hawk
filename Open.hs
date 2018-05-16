{-# LANGUAGE RecordWildCards #-}

module Open
  ( hawkOpen
  , globalOpen
  ) where

import           Control.Monad (forM, forM_, void)
import qualified Data.Aeson as J
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Maybe (isNothing, isJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import           Database.PostgreSQL.Typed (pgConnect, pgDisconnect)
import           System.FilePath (takeExtension)
import qualified System.Info as SI

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit2 as WK

import Paths_hawk (getDataFileName)
import Types
import Config
import GValue
import Cookies
import Bind
import UI
import JS
import Script
import qualified Data.PrefixMap as PM
import qualified Data.BitSet as ES
import URI
import Domain
import Scheme
import Event

toHex :: Double -> BSB.Builder
toHex x
  | x <= 0 = "00"
  | x >= 1 = "ff"
  | otherwise = BSB.word8HexFixed $ floor (255*x)
  where

globalOpen :: IO Global
globalOpen = do
  vmaj <- WK.getMajorVersion
  vmin <- WK.getMinorVersion
  let globalUserAgent = T.pack $ "hawk (X11; " <> SI.os <> " " <> SI.arch <> ") WebKit/" <> show vmaj <> "." <> show vmin
  css <- TIO.readFile =<< getDataFileName "hawk.css"
  globalStyleSheet <- WK.userStyleSheetNew css WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing
  js <- TLIO.readFile =<< getDataFileName "hawk.js"
  globalScript <- WK.userScriptNew (TL.toStrict $ js <> jsconf) WK.UserContentInjectedFramesAllFrames WK.UserScriptInjectionTimeStart Nothing Nothing
  return Global{..}
  where
  jsconf = TLB.toLazyText $ TLB.singleton '\n' <> setPropertiesBuilder
    [ ("loadSet", JSON $ J.object [ loadElementName l J..= ES.singleton l | l <- [minBound..maxBound] ])
    ]

hawkOpen :: Global -> Config -> IO Hawk
hawkOpen hawkGlobal@Global{..} hawkConfig@Config{..} = do
  hawkDatabase <- mapM pgConnect configDatabase

  hawkWindow <- G.new Gtk.Window
    [ #type G.:= Gtk.WindowTypeToplevel
    ]

  mapM_ (G.after hawkWindow #destroy . pgDisconnect) hawkDatabase

  hawkTopBox <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical ]
  #add hawkWindow hawkTopBox

  hawkStatusBox <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal ]
  #setSizeRequest hawkStatusBox (-1) 24
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
  _ <- setStyle hawkStatusURI "*{color:#08f;}"
  #packEnd hawkStatusBox hawkStatusURI True True 0


  hawkSettings <- G.new WK.Settings
    [ #userAgent G.:= globalUserAgent
    ]
  forM_ (HM.toList configSettings) $ \(k, v) ->
    setObjectProperty hawkSettings k v

  hawkUserContentManager <- WK.userContentManagerNew

  hawkStyleSheets <- forM configStyleSheet $ \f -> do
    d <- TIO.readFile f
    WK.userStyleSheetNew d WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing

  hawkScript <- forM configScript $ \f -> do
    d <- TIO.readFile f
    WK.userScriptNew d WK.UserContentInjectedFramesAllFrames WK.UserScriptInjectionTimeStart Nothing Nothing

  hawkWebContext <- WK.webContextNewWithWebsiteDataManager =<<
    G.new WK.WebsiteDataManager
      (                 (:) (#isEphemeral        G.:= isNothing configDataDirectory)
      $ maybe id (\d -> (:) (#baseDataDirectory  G.:= T.pack d)) configDataDirectory
      $ maybe id (\d -> (:) (#baseCacheDirectory G.:= T.pack d)) configCacheDirectory
      [])

  hawkCookieManager <- #getCookieManager hawkWebContext
  forM_ configCookieFile $ \f ->
    #setPersistentStorage hawkCookieManager (T.pack f) (case takeExtension f of
      ".txt" -> WK.CookiePersistentStorageText
      "" -> WK.CookiePersistentStorageText
      _ -> WK.CookiePersistentStorageSqlite)

  #setCacheModel hawkWebContext configCacheModel
  #setWebProcessCountLimit hawkWebContext configProcessCountLimit
  forM_ configProxy $ \p ->
    #setNetworkProxySettings hawkWebContext WK.NetworkProxyModeCustom . Just
      =<< WK.networkProxySettingsNew (Just p) (Just configProxyIgnore)
  #setSpellCheckingEnabled hawkWebContext configSpellChecking
  #setProcessModel hawkWebContext configProcessModel


  hawkWebView <- G.new WK.WebView
    [ #webContext G.:= hawkWebContext
    , #settings G.:= hawkSettings
    , #userContentManager G.:= hawkUserContentManager
    , #editable G.:= configEditable
    , #zoomLevel G.:= configZoomLevel
    ]
  #setCustomCharset hawkWebView configCharset
  #packStart hawkTopBox hawkWebView True True 0

  hawkClipboard <- Gtk.clipboardGet {- Gdk.SELECTION_PRIMARY: wrapPtr Gdk.Atom (intPtrToPtr 1) -} =<< Gdk.atomInternStaticString "PRIMARY"

  hawkURIDomain <- newIORef "."
  hawkBindings <- newIORef def
  hawkStyleSheet <- newIORef undefined
  hawkPromptHistory <- newIORef HM.empty
  hawkSiteOverride <- newIORef mempty

  let hawk = Hawk{..}
      run = runHawkM hawk

  _ <- G.after hawkWebView #close $ #destroy hawkWindow

  _ <- G.on hawkWebView #loadChanged $ \ev ->
    #setText hawkStatusLoad =<< case ev of
      WK.LoadEventStarted     -> "WAIT"  <$ run loadStarted
      WK.LoadEventRedirected  -> "REDIR" <$ run loadStarted
      WK.LoadEventCommitted   -> "RECV"  <$ run loadCommitted
      WK.LoadEventFinished    -> ""      <$ run loadFinished
      (WK.AnotherLoadEvent x) -> return $ T.pack $ show x
  _ <- G.on hawkWebView #loadFailed $ \_ _ _ -> do
    #setText hawkStatusLoad "ERR"
    return False

  _ <- G.on hawkWebView (G.PropertyNotify #estimatedLoadProgress) $ \_ -> do
    p <- #getEstimatedLoadProgress hawkWebView
    #loadFromData hawkStatusLoadStyle $ BSL.toStrict $ BSB.toLazyByteString $ "*{background-color:#" <> toHex (1-p) <> toHex p <> "00;}"

  _ <- G.on hawkWebView (G.PropertyNotify #uri) $ \_ -> do
    uri <- #getUri hawkWebView
    run $ uriChanged uri
    #setText hawkStatusURI $ fold uri
  _ <- G.on hawkWebView (G.PropertyNotify #title) $ \_ ->
    #setTitle hawkWindow . ("hawk " <>) . fold =<< #getTitle hawkWebView
  _ <- G.on hawkWebView #mouseTargetChanged $ (.) run . targetChanged

  if isJust (PM.lookup [] configTLSAccept)
    then #setTlsErrorsPolicy hawkWebContext WK.TLSErrorsPolicyIgnore
    else void $ G.on hawkWebView #loadFailedWithTlsErrors $ \u c f -> case uriDomain u of
      Just d | isJust (PM.lookupPrefix (domainComponents d) configTLSAccept) -> do
        putStrLn $ "Accepting TLS Certificate: " ++ show d ++ " " ++ show f
        #allowTlsCertificateForHost hawkWebContext c $ joinDomain d
        return True
      _ -> do
        putStrLn $ "Rejecting TLS Certificate: " ++ show u ++ " " ++ show f
        return False

  hawkFindController <- #getFindController hawkWebView
  _ <- G.on hawkFindController #foundText $ \n ->
    #setText hawkStatusLeft $ T.pack $ (if n == maxBound then "many" else show n) ++ " match" ++ (if n == 1 then "" else "es")
  _ <- G.on hawkFindController #failedToFindText $
    #setText hawkStatusLeft $ T.pack $ "no matches"

  #registerUriScheme hawkWebContext "hawk" $ run . hawkURIScheme
  _ <- G.on hawkWindow #keyPressEvent $ run . runBind
  -- TODO: add signal detail haskell-gi #158
  _ <- G.on hawkUserContentManager #scriptMessageReceived $ run . scriptMessageHandler
  True <- #registerScriptMessageHandler hawkUserContentManager "hawk"

  run $ do
    loadStyleSheet 0
    loadCookies
    uriChanged Nothing
    mapM_ hawkGoto configURI

  #showAll hawkWindow
  return hawk
