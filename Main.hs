{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad (forM)
import           Data.Default (Default(def))
import qualified Data.GI.Base as G
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Vector as V
import qualified System.Console.GetOpt as GetOpt
import           System.Directory (getAppUserDataDirectory)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)

import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

import Paths_hawk (getDataFileName)
import Types
import Open
import Cookies
import Database

data Opts = Opts
  { optCookies :: Maybe FilePath
  , optDatabase :: Maybe Database
  }

instance Default Opts where
  def = Opts
    { optCookies = Just "cookies.txt"
    , optDatabase = Just defaultDatabase
    }

optDescrs :: [GetOpt.OptDescr (Opts -> Opts)]
optDescrs =
  [ GetOpt.Option "c" ["cookies"]
    (GetOpt.OptArg (\f o -> o{ optCookies = f }) "FILE")
    ("Load and use cookies from FILE [" ++ fromMaybe "NONE" (optCookies def) ++ "]")
  , GetOpt.Option "n" ["no-database"]
      (GetOpt.NoArg (\o -> o{ optDatabase = Nothing }))
      "do not connect to a database"
  ]

main :: IO ()
main = do
  Just (prog:args) <- Gtk.init . Just . map T.pack =<< (:) <$> getProgName <*> getArgs

  (Opts{..}, urls) <- case GetOpt.getOpt GetOpt.Permute optDescrs (map T.unpack args) of
    (o, a, []) -> return (foldl' (flip ($)) def o, a)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ GetOpt.usageInfo (T.unpack prog ++ " [OPTIONS] [URI ...]") optDescrs
      exitFailure
  dir <- getAppUserDataDirectory "hawk"

  db <- mapM databaseOpen optDatabase

  cookies <- maybe (return emptyCookies) (loadCookiesTxt . (dir </>)) optCookies

  ctx <- WK.webContextNewEphemeral
  #setProcessModel ctx WK.ProcessModelMultipleSecondaryProcesses
  cm <- #getCookieManager ctx
  -- #setPersistentStorage cm "/tmp/hawk-cookies.txt" WK.CookiePersistentStorageText
  addCookiesTo cm cookies

  css <- forM ["plain.css", "style.css"] $ \f -> do
    d <- T.IO.readFile =<< getDataFileName f
    WK.userStyleSheetNew d WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing

  let global = Global
        { globalWebContext = ctx
        , globalStyleSheets = V.fromList css
        , globalDatabase = db
        }

  hawk <- hawkOpen global

  _ <- G.after (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main

  mapM_ databaseClose db
