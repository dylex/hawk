module Hawk
  ( Hawk(..)
  , HawkM
  , runHawkM
  , asksState
  , hawkOpen
  , hawkClose
  , hawkGoto
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Default (def)
import qualified Data.GI.Base as G
import qualified Data.Text as T

import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.WebKit2 as WK

import State

data Hawk = Hawk
  { hawkWindow :: !Gtk.Window
  , hawkWebView :: !WK.WebView
  , hawkStatusLeft, hawkStatusRight :: !Gtk.Label
  , hawkStatusPrompt :: !Gtk.Entry
  , hawkState :: !(MVar State)
  }

type HawkM = ReaderT Hawk IO

runHawkM :: Hawk -> HawkM a -> IO a
runHawkM = flip runReaderT

asksState :: (State -> a) -> HawkM a
asksState g =
  fmap g . liftIO . readMVar =<< asks hawkState

hawkOpen :: WK.Settings -> IO Hawk
hawkOpen settings = do
  win <- G.new Gtk.Window
    [ #type G.:= Gtk.WindowTypeToplevel
    ]
  _ <- G.on win #destroy Gtk.mainQuit

  box <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationVertical
    ]
  #add win box

  wv <- G.new WK.WebView
    [ #settings G.:= settings
    ]
  #packStart box wv True True 0

  status <- G.new Gtk.Box
    [ #orientation G.:= Gtk.OrientationHorizontal
    ]
  #packStart box status False False 0

  left <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignStart
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #setMarkup left "Loading..."
  #packStart box left False False 0

  prompt <- G.new Gtk.Entry
    [ #halign G.:= Gtk.AlignStart
    ]
  #packStart box prompt True True 0

  right <- G.new Gtk.Label
    [ #halign G.:= Gtk.AlignEnd
    , #selectable G.:= True
    , #ellipsize G.:= Pango.EllipsizeModeEnd
    ]
  #packStart box right True False 0

  #showAll win
  #hide prompt

  state <- newMVar def

  return Hawk
    { hawkWindow = win
    , hawkWebView = wv
    , hawkStatusLeft = left
    , hawkStatusRight = right
    , hawkStatusPrompt = prompt
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
