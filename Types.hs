{-# LANGUAGE TupleSections #-}

module Types
  ( Global(..)
  , Bindings(..)
  , Hawk(..)
  , HawkM
  , runHawkM
  , asksGlobal
  , asksConfig
  , askWebView
  , askSettings
  , askWebContext
  , askUserContentManager
  , askWebsiteDataManager 
  , askCookieManager
  , askFindController
  , readRef
  , writeRef
  , modifyRef
  , modifyRefId
  , modifyRef_
  , noCancellable
  ) where

import           Control.Concurrent.MVar (MVar)
import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Default (Default(def))
import qualified Data.HashMap.Strict as HM
import           Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32)
import           Database.PostgreSQL.Typed (PGConnection)
import qualified Deque.Strict as D

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Domain
import Config
import Filter (Filters)

data Bindings
  = Command
    { commandCount :: Maybe Word32
    }
  | PassThru
    { passThruKey :: Word32
    , bindingsReturn :: HawkM Bindings
    }

instance Default Bindings where
  def = Command Nothing

type PromptHistory = IORef (D.Deque T.Text)

data Global = Global
  { hawkConfig :: !Config
  , hawkActive :: !(MVar Int)
  , globalUserAgent :: !T.Text
  , globalStyleSheet :: !WK.UserStyleSheet
  , globalScript :: !WK.UserScript
  
  , hawkDatabase :: !(Maybe PGConnection)
  , hawkClipboard :: !Gtk.Clipboard
  , hawkFilterStore :: !WK.UserContentFilterStore
  , hawkWebContext :: !WK.WebContext
  , hawkStyleSheets :: !(V.Vector WK.UserStyleSheet)
  , hawkScript :: !(Maybe WK.UserScript)
  }

data Hawk = Hawk
  { hawkGlobal :: !Global
  , hawkWindow :: !Gtk.Window
  , hawkStatusBox :: !Gtk.Box
  , hawkStatusStyle :: !Gtk.CssProvider
  , hawkStatusCount, hawkStatusLeft :: !Gtk.Label
  , hawkWebView :: !WK.WebView

  , hawkURIDomain :: !(IORef Domain)
  , hawkBindings :: !(IORef Bindings)
  , hawkStyleSheet :: !(IORef Int)
  , hawkPromptHistory :: !(IORef (HM.HashMap T.Text PromptHistory))
  , hawkSiteOverride :: !(IORef SiteConfig)
  , hawkFilters :: !(IORef Filters)
  }

type HawkM = ReaderT Hawk IO

runHawkM :: Hawk -> HawkM a -> IO a
runHawkM = flip runReaderT

asksGlobal :: (Global -> a) -> HawkM a
asksGlobal = asks . (. hawkGlobal)

asksConfig :: (Config -> a) -> HawkM a
asksConfig = asksGlobal . (. hawkConfig)

askWebView :: HawkM WK.WebView
askWebView = asks hawkWebView

askSettings :: HawkM WK.Settings
askSettings = #getSettings =<< askWebView

askWebContext :: HawkM WK.WebContext
askWebContext = asksGlobal hawkWebContext

askUserContentManager :: HawkM WK.UserContentManager
askUserContentManager = #getUserContentManager =<< askWebView

askWebsiteDataManager :: HawkM WK.WebsiteDataManager
askWebsiteDataManager = #getWebsiteDataManager =<< askWebView

askCookieManager :: HawkM WK.CookieManager
askCookieManager = #getCookieManager =<< askWebsiteDataManager

askFindController :: HawkM WK.FindController
askFindController = #getFindController =<< askWebView

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

modifyRefId :: (Hawk -> IORef a) -> (a -> a) -> HawkM a
modifyRefId f = modifyRef f . (join (,) .)

modifyRef_ :: (Hawk -> IORef a) -> (a -> a) -> HawkM ()
modifyRef_ f = modifyRef f . ((, ()) .)

noCancellable :: Maybe Gio.Cancellable
noCancellable = Nothing
