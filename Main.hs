{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad (foldM)
import qualified Data.GI.Base as G
import qualified Data.Text as T
import qualified System.Console.GetOpt as GetOpt
import           System.Directory (getAppUserDataDirectory, setCurrentDirectory)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import qualified GI.Gtk as Gtk

import Types
import Config
import Open

optDescrs :: [GetOpt.OptDescr (Config -> IO Config)]
optDescrs =
  [ GetOpt.Option "c" ["config"]
    (GetOpt.ReqArg (flip parseConfigFile) "FILE")
    "Use the given configuration file (relative to ~/.hawk)"
  , GetOpt.Option "n" ["no-database"]
    (GetOpt.NoArg (\c -> return c{ configDatabase = Nothing }))
    "do not connect to a database"
  ]

optArgs :: String -> Config -> IO Config
optArgs u c = return c{ configURI = Just $ T.pack u }

main :: IO ()
main = do
  Just (prog:args) <- Gtk.init . Just . map T.pack =<< (:) <$> getProgName <*> getArgs

  setCurrentDirectory =<< getAppUserDataDirectory "hawk"
  conf <- baseConfig

  config <- case GetOpt.getOpt (GetOpt.ReturnInOrder optArgs) optDescrs (map T.unpack args) of
    (o, [], []) -> foldM (flip ($)) conf o
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ GetOpt.usageInfo (T.unpack prog ++ " [OPTIONS] [URI]") optDescrs
      exitFailure

  hawk <- hawkOpen config

  _ <- G.after (hawkWindow hawk) #destroy Gtk.mainQuit

  Gtk.main
