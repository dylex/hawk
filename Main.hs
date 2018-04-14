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
  { optConfig :: Maybe FilePath
  , optDatabase :: Maybe Database
  }

instance Default Opts where
  def = Opts
    { optConfig = Nothing
    , optDatabase = Just def
    }

optDescrs :: [GetOpt.OptDescr (Opts -> Opts)]
optDescrs =
  [ GetOpt.Option "c" ["config"]
    (GetOpt.OptArg (\f o -> o{ optConfig = f }) "FILE")
    "Use the given configuration file"
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

  hawk <- hawkOpen config

  _ <- G.after (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main

  mapM_ databaseClose db
