{-# LANGUAGE DataKinds #-}
module Open
  ( hawkOpen
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import           Data.IORef (newIORef)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit2 as WK

import Settings
import Types
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

hawkOpen :: Global -> IO Hawk
hawkOpen global = do
  win <- G.new Gtk.Window
    [ #type G.:= Gtk.WindowTypeToplevel
    ]

  top <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical
    ]
  #add win top

  settings <- G.new WK.Settings defaultSettings
  usercm <- WK.userContentManagerNew
  #addStyleSheet usercm $ V.head $ globalStyleSheets global
  wv <- G.new WK.WebView
    [ #webContext G.:= globalWebContext global
    , #settings G.:= settings
    , #userContentManager G.:= usercm
    ]
  #packStart top wv True True 0

  status <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal
    ]
  statuscss <- setStyle status "*{}"
  #packStart top status False False 0

  -- status left
  count <- G.new Gtk.Label []
  _ <- setStyle count "*{background-color:#8cc;color:#000;}"
  #packStart status count False False 0

  left <- G.new Gtk.Label
    [ #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #setMarkup left "Loading..."
  #packStart status left False False 0

  -- status right
  load <- G.new Gtk.Label []
  _ <- setStyle load "*{color:#fff;}"
  loadcss <- setStyle load "*{}"
  #packEnd status load False False 0

  _ <- G.on wv #loadChanged $ \ev ->
    Gtk.labelSetText load $ case ev of
      WK.LoadEventStarted -> "WAIT"
      WK.LoadEventRedirected -> "REDIR"
      WK.LoadEventCommitted -> "RECV"
      WK.LoadEventFinished -> ""
      (WK.AnotherLoadEvent x) -> T.pack $ show x

  _ <- G.on wv (G.PropertyNotify #estimatedLoadProgress) $ \_ -> do
    p <- G.get wv #estimatedLoadProgress
    #loadFromData loadcss $ BSL.toStrict $ BSB.toLazyByteString $ "*{background-color:#" <> toHex (1-p) <> toHex p <> "00;}"

  loc <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignEnd
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #packEnd status loc True True 0

  _ <- G.on wv (G.PropertyNotify #uri) $ \_ ->
    Gtk.labelSetText loc . fold =<< G.get wv #uri

  _ <- G.on wv #close $ #destroy win

  state <- newIORef def
  let hawk = Hawk
        { hawkGlobal = global
        , hawkWindow = win
        , hawkWebView = wv
        , hawkSettings = settings
        , hawkUserCM = usercm
        , hawkStatusBox = status
        , hawkStatusStyle = statuscss
        , hawkStatusCount = count
        , hawkStatusLeft = left
        , hawkState = state
        }

  _ <- G.on win #keyPressEvent $ runHawkM hawk . runBind

  #showAll win

  return hawk
