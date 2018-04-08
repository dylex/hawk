module Main (main) where

import           Control.Monad (forM)
import qualified Data.GI.Base as G
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Vector as V
import           System.Environment (getProgName, getArgs)

import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Paths_hawk (getDataFileName)
import Bind
import State
import Hawk

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs

  Just (_prog:_args) <- Gtk.init $ Just $ map T.pack $ prog : args

  ctx <- WK.webContextNewEphemeral

  css <- forM ["plain.css", "style.css"] $ \f -> do
    d <- T.IO.readFile =<< getDataFileName f
    WK.userStyleSheetNew d WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing

  let global = Global
        { globalWebContext = ctx
        , globalStyleSheets = V.fromList css
        }

  hawk <- hawkOpen global
  _ <- G.on (hawkWindow hawk) #keyPressEvent $ runHawkM hawk . runBind

  _ <- G.after (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main
