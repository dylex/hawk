{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad (foldM)
import           Data.Default (def)
import           Data.Function ((&))
import qualified Data.GI.Base as G
import qualified Data.Text as T
import qualified System.Console.GetOpt as GetOpt
import           System.Directory (getAppUserDataDirectory, setCurrentDirectory)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import qualified GI.Gtk as Gtk

import Paths_hawk (getDataFileName)
import Util
import Types
import Config
import Open

optDescrs :: [GetOpt.OptDescr (Config -> IO Config)]
optDescrs =
  [ GetOpt.Option "c" ["config"]
    (GetOpt.ReqArg (flip loadConfigFile) "FILE")
    "Load configuration file (relative to base)"
  , GetOpt.Option "n" ["no-database"]
    (GetOpt.NoArg (\c -> return c{ configDatabase = Nothing }))
    "Do not connect to a database"
  ]

optArgs :: String -> Config -> IO Config
optArgs u c = return c{ configURI = Just $ T.pack u }

main :: IO ()
main = do
  Just (prog:args) <- Gtk.init . Just . map T.pack =<< (:) <$> getProgName <*> getArgs

  -- load system config
  base <- loadConfigFile def =<< getDataFileName baseConfigFile
  global <- globalOpen

  -- load user config
  setCurrentDirectory =<< getAppUserDataDirectory "hawk"
  conf <- fromDoesNotExist base $ loadConfigFile base baseConfigFile

  config <- case GetOpt.getOpt (GetOpt.ReturnInOrder optArgs) optDescrs (map T.unpack args) of
    (o, [], []) -> foldM (&) conf o
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ GetOpt.usageInfo (T.unpack prog ++ " [OPTIONS] [URI]") optDescrs
      exitFailure

  hawk <- hawkOpen global config

  _ <- G.after (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main
