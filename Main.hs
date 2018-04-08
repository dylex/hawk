module Main (main) where

import qualified Data.GI.Base as G
import qualified Data.Text as T
import           System.Environment (getProgName, getArgs)

import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Bind
import Hawk

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs

  Just (_prog:_args) <- Gtk.init $ Just $ map T.pack $ prog : args

  ctx <- WK.webContextNewEphemeral

  hawk <- hawkOpen ctx

  _ <- G.on (hawkWindow hawk) #keyPressEvent $ runHawkM hawk . runBind

  _ <- G.after (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main
