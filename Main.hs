{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad (foldM)
import           Data.Char (isAlphaNum)
import           Data.Default (def)
import           Data.Function ((&))
import qualified Data.GI.Base as G
import           Data.Int (Int32)
import qualified Data.Text as T
import           System.Directory (getAppUserDataDirectory, setCurrentDirectory)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitWith, ExitCode(..))

import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Paths_hawk (getDataFileName)
import Util
import Config
import Expand
import Open

optArg :: Config -> String -> IO Config
optArg c u
  | all (\x -> isAlphaNum x || '_' == x) u = fromDoesNotExist cu $ loadConfigFile c u
  | otherwise = return cu
  where
  cu = c{ configURI = Just $ expandURIWith c $ T.pack u }

start :: (?self :: Gtk.Application) => Config -> Gio.ApplicationCommandLine -> IO Int32
start conf0 cmd = do
  opts <- #getOptionsDict cmd
  file <- maybe (return Nothing) G.fromGVariant =<< #lookupValue opts "config" Nothing
  nodb <- #contains opts "no-database"
  _prog:args <- #getArguments cmd

  conf <- foldM (&) conf0
    [ \c -> foldM (\d -> loadConfigFile d . T.unpack) c file
    , \c -> foldM optArg c args
    , \c -> return $ if nodb then c{ configDatabase = Nothing } else c
    ]

  global <- globalOpen ?self conf
  _ <- G.on ?self #shutdown $ globalClose global
  hawk <- hawkOpen global Nothing
  hawkShow hawk

  return 0

main :: IO ()
main = do
  -- load system config
  base <- loadConfigFile def =<< getDataFileName baseConfigFile

  -- load user config
  setCurrentDirectory =<< getAppUserDataDirectory "hawk"
  conf <- fromDoesNotExist base (loadConfigFile base baseConfigFile)

  appl <- Gtk.applicationNew (Just "net.dylex.hawk") [Gio.ApplicationFlagsHandlesCommandLine, Gio.ApplicationFlagsNonUnique]
  #addMainOption appl "no-database" (fromIntegral $ fromEnum 'n') [] GLib.OptionArgNone "Do not connect to a database" Nothing
  #addMainOption appl "config" (fromIntegral $ fromEnum 'c') [] GLib.OptionArgFilename "Load configuration file (relative to base)" (Just "FILE")
  _ <- G.on appl #commandLine $ start conf

  prog <- getProgName
  args <- getArgs
  res <- #run appl (Just (prog:args))
  exitWith (if res == 0 then ExitSuccess else ExitFailure (fromIntegral res))
