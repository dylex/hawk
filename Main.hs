module Main (main) where

import qualified Data.GI.Base as G
import qualified Data.Text as T
import           System.Environment (getProgName, getArgs)

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Bind
import Settings
import Hawk

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs

  Just (_prog:_args) <- Gtk.init $ Just $ map T.pack $ prog : args

  settings <- G.new WK.Settings defaultSettings

  hawk <- hawkOpen settings

  _ <- G.on (hawkWindow hawk) #keyPressEvent $ runHawkM hawk . runBind
  _ <- G.on (hawkWebView hawk) #loadChanged $ \ev -> do
    print ev

  _ <- G.on (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main