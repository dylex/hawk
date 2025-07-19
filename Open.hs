{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Open
  ( hawkOpen
  , hawkShow
  , globalOpen
  , globalClose
  ) where

import           Control.Monad (forM, forM_, when, void)
import qualified Data.Aeson as J
import           Data.Default (def)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Maybe (isNothing, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import           Database.PostgreSQL.Typed (pgConnect, pgDisconnect)
import           Numeric (showHex)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeExtension)
import qualified System.Info as SI

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit as WK

import Paths_hawk (getDataFileName)
import Types
import Config
import GValue
import Cookies
import Bind
import UI
import Script
import Content
import qualified Data.PrefixMap as PM
import URI
import Domain
import Scheme
import Event

toHex :: Double -> String
toHex x
  | n <= 0 = "00"
  | x >= 1 = "ff"
  | n < 16 = '0':s
  | otherwise = s
  where
  n = floor (255*x)
  s = showHex (n :: Int) ""

globalOpen :: Gtk.Application -> Config -> IO Global
globalOpen hawkApplication hawkConfig@Config{..} = do
  vmaj <- WK.getMajorVersion
  vmin <- WK.getMinorVersion
  let globalUserAgent = T.pack $ "hawk (X11; " <> SI.os <> " " <> SI.arch <> ") WebKit/" <> show vmaj <> "." <> show vmin
  css <- TIO.readFile =<< getDataFileName "hawk.css"
  globalStyleSheet <- WK.userStyleSheetNew css WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing
  js <- TLIO.readFile =<< getDataFileName "hawk.js"
  globalScript <- WK.userScriptNew (TL.toStrict js) WK.UserContentInjectedFramesAllFrames WK.UserScriptInjectionTimeStart Nothing Nothing

  hawkDatabase <- mapM pgConnect configDatabase

  hawkStyleSheets <- forM configStyleSheet $ \f -> do
    d <- TIO.readFile f
    WK.userStyleSheetNew d WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing

  hawkScript <- forM configScript $ \f -> do
    d <- TIO.readFile f
    WK.userScriptNew d WK.UserContentInjectedFramesAllFrames WK.UserScriptInjectionTimeStart Nothing Nothing

  hawkFilterStore <- WK.userContentFilterStoreNew $ T.pack configFilterDirectory 

  hawkWebContext <- WK.webContextGetDefault
  hawkNetworkSession <- if isNothing configDataDirectory
    then WK.networkSessionNewEphemeral
    else WK.networkSessionNew (T.pack <$> configDataDirectory) (T.pack <$> configCacheDirectory)

  hawkCookieManager <- #getCookieManager hawkNetworkSession
  forM_ configCookieFile $ \f ->
    #setPersistentStorage hawkCookieManager (T.pack f) (case takeExtension f of
      ".txt" -> WK.CookiePersistentStorageText
      "" -> WK.CookiePersistentStorageText
      _ -> WK.CookiePersistentStorageSqlite)

  #setCacheModel hawkWebContext configCacheModel
  forM_ configProxy $ \p ->
    #setProxySettings hawkNetworkSession WK.NetworkProxyModeCustom . Just
      =<< WK.networkProxySettingsNew (Just p) (Just configProxyIgnore)
  #setSpellCheckingEnabled hawkWebContext configSpellChecking
  -- #setProcessModel hawkWebContext configProcessModel

  display <- maybe (fail "no display") return =<< Gdk.displayGetDefault
  hawkClipboard <- Gdk.displayGetPrimaryClipboard display

  return Global{..}

globalClose :: Global -> IO ()
globalClose Global{..} = do
  mapM_ pgDisconnect hawkDatabase

hawkOpen :: Global -> Maybe Hawk -> IO Hawk
hawkOpen hawkGlobal@Global{..} parent = do
  let Config{..} = hawkConfig

  hawkWindow <- G.new Gtk.Window
    [ #application G.:= hawkApplication ]

  hawkTopBox <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical ]
  #setChild hawkWindow (Just hawkTopBox)

  hawkStatusBox <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal ]
  #setSizeRequest hawkStatusBox (-1) 26
  hawkStatusStyle <- setStyle hawkStatusBox "*{font-size:26pt;}"
  #append hawkTopBox hawkStatusBox

  -- status left
  hawkStatusCount <- G.new Gtk.Label []
  _ <- setStyle hawkStatusCount "*{background-color:#8cc;color:#000;}"
  #append hawkStatusBox hawkStatusCount

  hawkStatusLeft <- G.new Gtk.Label
    [ #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #append hawkStatusBox hawkStatusLeft

  -- status right
  hawkStatusURI <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignEnd
    , #hexpand G.:= True
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  _ <- setStyle hawkStatusURI "*{color:#08f;}"
  #append hawkStatusBox hawkStatusURI

  hawkStatusLoad <- G.new Gtk.Label []
  _ <- setStyle hawkStatusLoad "*{color:#fff;}"
  hawkStatusLoadStyle <- setStyle hawkStatusLoad "*{}"
  #append hawkStatusBox hawkStatusLoad

  hawkSettings <- G.new WK.Settings
    [ #userAgent G.:= globalUserAgent
    ]
  mapM_ (uncurry $ setObjectProperty hawkSettings) $ HM.toList $ configSettings $ defaultSiteConfig hawkConfig

  hawkUserContentManager <- WK.userContentManagerNew

  hawkWebView <- G.new WK.WebView $
    [ #webContext G.:= hawkWebContext
    , #networkSession G.:= hawkNetworkSession
    , #settings G.:= hawkSettings
    , #userContentManager G.:= hawkUserContentManager
    , #editable G.:= configEditable
    , #zoomLevel G.:= configZoomLevel
    , #vexpand G.:= True
    ] ++ maybe [] (return . (#relatedView G.:=) . hawkWebView) parent
  #setCustomCharset hawkWebView configCharset
  #prepend hawkTopBox hawkWebView

  hawkURIDomain <- newIORef "."
  hawkBindings <- newIORef def
  hawkStyleSheet <- newIORef undefined
  hawkPromptHistory <- newIORef HM.empty
  hawkSiteOverride <- newIORef mempty
  hawkFilters <- newIORef $ configFilters hawkConfig

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
    #loadFromString hawkStatusLoadStyle $ "*{background-color:#" <> T.pack (toHex (1-p) <> toHex p) <> "00;}"

  _ <- G.on hawkWebView (G.PropertyNotify #uri) $ \_ -> do
    uri <- #getUri hawkWebView
    -- print $ "changed: " <> show uri
    run $ uriChanged uri
    #setText hawkStatusURI $ fold uri
  _ <- G.on hawkWebView (G.PropertyNotify #title) $ \_ ->
    #setTitle hawkWindow . Just . ("hawk " <>) . fold =<< #getTitle hawkWebView
  _ <- G.on hawkWebView #mouseTargetChanged $ (.) run . targetChanged

  _ <- G.on hawkWebView #decidePolicy $ \d t -> do
   -- print $ "decide " <> show t
   case t of
    {-
    WK.PolicyDecisionTypeResponse -> do
      rd <- G.unsafeCastTo WK.ResponsePolicyDecision d
      req <- G.get rd #request
      resp <- G.get rd #response
      uri <- G.get req #uri
      mime <- G.get resp #mimeType
      print (uri, mime)
      return False
    -}
    WK.PolicyDecisionTypeNewWindowAction -> do
      #ignore d
      {-
      nd <- G.unsafeCastTo WK.NavigationPolicyDecision d
      req <- G.get nd #request
      print =<< G.get req #uri
      #loadRequest hawkWebView req
      -}
      return True
    _ -> return False

  if isJust (PM.lookup [] configTLSAccept)
    then #setTlsErrorsPolicy hawkNetworkSession WK.TLSErrorsPolicyIgnore
    else void $ G.on hawkWebView #loadFailedWithTlsErrors $ \u c f -> case uriDomain u of
      Just d | isJust (PM.lookupPrefix (domainComponents d) configTLSAccept) -> do
        putStrLn $ "Accepting TLS Certificate: " ++ show d ++ " " ++ show f
        -- this doesn't work for some reason:
        #allowTlsCertificateForHost hawkNetworkSession c $ joinDomain d
        return False
      _ -> do
        putStrLn $ "Rejecting TLS Certificate: " ++ show u ++ " " ++ show f
        return False

  mapM_ (createDirectoryIfMissing False) configDownloadDir
  _ <- G.on hawkNetworkSession #downloadStarted $ run . startDownload

  hawkFindController <- #getFindController hawkWebView
  _ <- G.on hawkFindController #foundText $ \n ->
    #setText hawkStatusLeft $ T.pack $ (if n == maxBound then "many" else show n) ++ " match" ++ (if n == 1 then "" else "es")
  _ <- G.on hawkFindController #failedToFindText $
    #setText hawkStatusLeft $ T.pack $ "no matches"

  #registerUriScheme hawkWebContext "hawk" $ run . hawkURIScheme

  ec <- G.new Gtk.EventControllerKey
    [ #propagationLimit G.:= Gtk.PropagationLimitNone
    , #propagationPhase G.:= Gtk.PropagationPhaseCapture
    , G.On #keyPressed $ \kv kc state -> run $ runBind kv kc state
    ]
  #addController hawkWindow ec
  -- _ <- G.on hawkWindow #buttonPressEvent $ run . runMouse
  _ <- G.on hawkUserContentManager (#scriptMessageReceived G.::: "hawk") $ run . scriptMessageHandler
  True <- #registerScriptMessageHandler hawkUserContentManager "hawk" Nothing

  {-
  _ <- G.on hawkWindow #scrollEvent $ \e -> do
    t <- G.get e #type
    print t
    return False
  -}

  _ <- G.on hawkWebView #create $ \nav -> do
    print =<< #getNavigationType nav
    child@Hawk{ hawkWebView = wv } <- hawkOpen hawkGlobal{ hawkConfig = hawkConfig{ configURI = Nothing } } (Just hawk)
    _ <- G.on wv #readyToShow $
      hawkShow child
    Gtk.toWidget =<< G.withManagedPtr wv (G.wrapObject WK.WebView)

  run $ do
    when (configContentFilter /= J.Null) $
      addContentFilter "user" configContentFilter (return ())

    loadScripts
    loadStyleSheet 0
    loadCookies
    uriChanged Nothing
    updateFilters $
      mapM_ hawkGoto configURI

  return hawk

hawkShow :: Hawk -> IO ()
hawkShow Hawk{..} = do
  wp <- #getWindowProperties hawkWebView
  geom <- #getGeometry wp
  {-
  x <- G.get geom #x
  y <- G.get geom #y
  #move hawkWindow x y
  -}
  w <- G.get geom #width
  h <- G.get geom #height
  #setDefaultSize hawkWindow (if w == 0 then 640 else w) (if h == 0 then 480 else h)
  #present hawkWindow

