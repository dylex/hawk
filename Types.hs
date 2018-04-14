{-# LANGUAGE TupleSections #-}

module Types
  ( Bindings(..)
  , Hawk(..)
  , HawkM
  , runHawkM
  , asksSettings
  , asksWebContext
  , asksUserContentManager
  , asksWebsiteDataManager 
  , asksCookieManager
  , readRef
  , writeRef
  , modifyRef
  , modifyRef_
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Default (Default(def))
import           Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Vector as V
import           Data.Word (Word32)
import           Database.PostgreSQL.Typed (PGConnection)

import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Config

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

data Hawk = Hawk
  { hawkConfig :: !Config
  , hawkDatabase :: !(Maybe PGConnection)
  , hawkWindow :: !Gtk.Window
  , hawkStatusBox :: !Gtk.Box
  , hawkStatusStyle :: !Gtk.CssProvider
  , hawkStatusCount, hawkStatusLeft :: !Gtk.Label
  , hawkWebView :: !WK.WebView

  , hawkBindings :: !(IORef Bindings)
  , hawkStyleSheets :: !(V.Vector WK.UserStyleSheet)
  , hawkStyleSheet :: !(IORef Int)
  , hawkPrivateMode :: !(IORef Bool)
  }

type HawkM = ReaderT Hawk IO

runHawkM :: Hawk -> HawkM a -> IO a
runHawkM = flip runReaderT

asksSettings :: HawkM WK.Settings
asksSettings = #getSettings =<< asks hawkWebView

asksWebContext :: HawkM WK.WebContext
asksWebContext = #getContext =<< asks hawkWebView

asksUserContentManager :: HawkM WK.UserContentManager
asksUserContentManager = #getUserContentManager =<< asks hawkWebView

asksWebsiteDataManager :: HawkM WK.WebsiteDataManager
asksWebsiteDataManager = #getWebsiteDataManager =<< asks hawkWebView

asksCookieManager :: HawkM WK.CookieManager
asksCookieManager = #getCookieManager =<< asksWebsiteDataManager

readRef :: (Hawk -> IORef a) -> HawkM a
readRef f =
  liftIO . readIORef =<< asks f

writeRef :: (Hawk -> IORef a) -> a -> HawkM ()
writeRef f x = do
  v <- asks f
  liftIO $ writeIORef v x

modifyRef :: (Hawk -> IORef a) -> (a -> (a, b)) -> HawkM b
modifyRef f m = do
  v <- asks f
  liftIO $ atomicModifyIORef' v m

modifyRef_ :: (Hawk -> IORef a) -> (a -> a) -> HawkM ()
modifyRef_ f = modifyRef f . ((, ()) .)
