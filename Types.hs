{-# LANGUAGE TupleSections #-}

module Types
  ( Global(..)
  , Bindings(..)
  , State(..)
  , Hawk(..)
  , HawkM
  , runHawkM
  , asksState
  , modifyState
  , modifyState_
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Default (Default(def))
import           Data.IORef (IORef, readIORef, modifyIORef', atomicModifyIORef')
import qualified Data.Vector as V
import           Data.Word (Word32)
import           Database.PostgreSQL.Typed (PGConnection)

import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

data Global = Global
  { globalWebContext :: !WK.WebContext
  , globalStyleSheets :: V.Vector WK.UserStyleSheet
  , globalDatabase :: Maybe PGConnection
  }

data Bindings
  = Command
    { commandCount :: Maybe Word32
    }
  | PassThru
    { bindingsReturn :: HawkM Bindings
    }
    {-
  | Prompt 
    { promptPrompt :: !String
    , promptInput :: !Input
    , promptCompleter :: Completer
    , promptExec :: Maybe String -> UzblM ()
    }
  | Capture
    { captureFun :: ModKey -> UzblM ()
    , bindingsReturn :: Bindings 
    }
    -}

instance Default Bindings where
  def = Command Nothing

data State = State
  { stateBindings :: !Bindings
  , stateStyleSheet :: !Int
  }

instance Default State where
  def = State
    { stateBindings = def
    , stateStyleSheet = 0
    }

data Hawk = Hawk
  { hawkGlobal :: !Global
  , hawkWindow :: !Gtk.Window
  , hawkWebView :: !WK.WebView
  , hawkSettings :: !WK.Settings
  , hawkUserCM :: !WK.UserContentManager
  , hawkStatusBox :: !Gtk.Box
  , hawkStatusStyle :: !Gtk.CssProvider
  , hawkStatusCount, hawkStatusLeft :: !Gtk.Label
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
modifyState_ = modifyState . ((, ()) .)
{-
modifyState_ f = do
  statev <- asks hawkState
  liftIO $ modifyIORef' statev f
-}
