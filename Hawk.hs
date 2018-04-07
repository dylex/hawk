{-# LANGUAGE DataKinds #-}
module Hawk
  ( Hawk(..)
  , HawkM
  , runHawkM
  , asksState
  , modifyState
  , hawkOpen
  , hawkClose
  , hawkGoto
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Default (def)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as G (AttrOpTag(AttrConstruct))
import qualified Data.Text as T

import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit2 as WK

import State

data Hawk = Hawk
  { hawkWindow :: !Gtk.Window
  , hawkWebView :: !WK.WebView
  , hawkStatusBox :: !Gtk.Box
  , hawkStatusStyle :: !Gtk.CssProvider
  , hawkStatusLeft, hawkStatusRight :: !Gtk.Label
  , hawkState :: !(IORef State)
  }

type HawkM = ReaderT Hawk IO

runHawkM :: Hawk -> HawkM a -> IO a
runHawkM = flip runReaderT

asksState :: (State -> a) -> HawkM a
asksState g =
  fmap g . liftIO . readIORef =<< asks hawkState

modifyState :: (State -> State) -> HawkM ()
modifyState f = do
  statev <- asks hawkState
  liftIO $ modifyIORef' statev f

hawkOpen :: [G.AttrOp WK.WebView 'G.AttrConstruct] -> IO Hawk
hawkOpen settings = do
  win <- G.new Gtk.Window
    [ #type G.:= Gtk.WindowTypeToplevel
    ]

  top <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical
    ]
  #add win top

  wv <- G.new WK.WebView settings
  #packStart top wv True True 0

  status <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal
    ]
  #packStart top status False False 0

  css <- Gtk.cssProviderNew
  style <- #getStyleContext status
  #addProvider style css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

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
    , hawkStatusBox = status
    , hawkStatusStyle = css
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
