{-# LANGUAGE DataKinds #-}
module Hawk
  ( Hawk(..)
  , HawkM
  , runHawkM
  , asksState
  , modifyState
  , modifyState_
  , hawkOpen
  , hawkClose
  , hawkGoto
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import qualified Data.ByteString as BS
import           Data.Default (def)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef', atomicModifyIORef')
import qualified Data.GI.Base as G
import qualified Data.Text as T

import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit2 as WK

import Settings
import State

data Hawk = Hawk
  { hawkWindow :: !Gtk.Window
  , hawkWebView :: !WK.WebView
  , hawkSettings :: !WK.Settings
  , hawkStatusBox :: !Gtk.Box
  , hawkStatusStyle :: !Gtk.CssProvider
  , hawkStatusCount, hawkStatusLeft, hawkStatusRight :: !Gtk.Label
  , hawkState :: !(IORef State)
  }

type HawkM = ReaderT Hawk IO

runHawkM :: Hawk -> HawkM a -> IO a
runHawkM = flip runReaderT

asksState :: (State -> a) -> HawkM a
asksState g =
  fmap g . liftIO . readIORef =<< asks hawkState

modifyState :: (State -> (State, a)) -> HawkM a
modifyState f = do
  statev <- asks hawkState
  liftIO $ atomicModifyIORef' statev f

modifyState_ :: (State -> State) -> HawkM ()
modifyState_ f = do
  statev <- asks hawkState
  liftIO $ modifyIORef' statev f

setStyle :: Gtk.IsWidget w => w -> BS.ByteString -> IO Gtk.CssProvider
setStyle obj rules = do
  css <- Gtk.cssProviderNew
  style <- Gtk.widgetGetStyleContext obj
  #addProvider style css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
  #loadFromData css rules
  return css

hawkOpen :: WK.WebContext -> IO Hawk
hawkOpen ctx = do
  win <- G.new Gtk.Window
    [ #type G.:= Gtk.WindowTypeToplevel
    ]

  top <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical
    ]
  #add win top

  settings <- G.new WK.Settings defaultSettings

  wv <- G.new WK.WebView
    [ #webContext G.:= ctx
    , #settings G.:= settings
    ]
  #packStart top wv True True 0

  status <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal
    ]
  statuscss <- setStyle status "*{}"
  #packStart top status False False 0

  count <- G.new Gtk.Label []
  _ <- setStyle count "*{background-color:#8cc;color:#000;}"
  #packStart status count False False 0

  left <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignStart
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #setMarkup left "Loading..."
  #packStart status left False False 0

  right <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignEnd
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #packEnd status right True False 0

  #showAll win

  state <- newIORef def

  return Hawk
    { hawkWindow = win
    , hawkWebView = wv
    , hawkSettings = settings
    , hawkStatusBox = status
    , hawkStatusStyle = statuscss
    , hawkStatusCount = count
    , hawkStatusLeft = left
    , hawkStatusRight = right
    , hawkState = state
    }

hawkClose :: HawkM ()
hawkClose = do
  win <- asks hawkWindow
  #destroy win

hawkGoto :: T.Text -> HawkM ()
hawkGoto url = do
  wv <- asks hawkWebView
  #loadUri wv url
