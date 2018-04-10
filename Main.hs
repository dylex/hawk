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
import Bind
import State
import Hawk
import Cookies

data Opts = Opts
  { optCookies :: Maybe FilePath
  }

instance Default Opts where
  def = Opts
    { optCookies = Just "cookies.txt"
    }

optDescrs :: [GetOpt.OptDescr (Opts -> Opts)]
optDescrs =
  [ GetOpt.Option "c" ["cookies"]
    (GetOpt.OptArg (\f o -> o{ optCookies = f }) "FILE")
    ("Load and use cookies from FILE [" ++ fromMaybe "NONE" (optCookies def) ++ "]")
  ]

main :: IO ()
main = do
  Just (prog:args) <- Gtk.init . Just . map T.pack =<< (:) <$> getProgName <*> getArgs

  (opts, urls) <- case GetOpt.getOpt GetOpt.Permute optDescrs (map T.unpack args) of
    (o, a, []) -> return (foldl' (flip ($)) def o, a)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ GetOpt.usageInfo (T.unpack prog ++ " [OPTIONS] [URI ...]") optDescrs
      exitFailure
  dir <- getAppUserDataDirectory "hawk"

  cookies <- maybe (return emptyCookies) (loadCookiesTxt . (dir </>)) $ optCookies opts

  ctx <- WK.webContextNewEphemeral
  #setProcessModel ctx WK.ProcessModelMultipleSecondaryProcesses
  addCookies ctx cookies

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
