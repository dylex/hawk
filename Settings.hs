{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Settings
  ( defaultSettings
  ) where

import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as G (AttrOpTag(AttrConstruct))
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import           System.Exit (ExitCode(ExitSuccess))
import qualified System.IO.Unsafe as Unsafe
import qualified System.Process as Proc

import qualified GI.WebKit2 as WK

captureLine :: FilePath -> [String] -> IO (Maybe T.Text)
captureLine cmd args = do
  (_, Just h, _, pid) <- Proc.createProcess (Proc.proc cmd args){ Proc.std_out = Proc.CreatePipe }
  out <- TIO.hGetLine h
  r <- Proc.waitForProcess pid
  return $ case r of
    ExitSuccess -> Just out
    _ -> Nothing

userAgents :: [T.Text]
userAgents =
  [ "hawk (X11; " <> uname <> ") WebKit/@{WEBKIT_MAJOR}.@{WEBKIT_MINOR}"
  , "Mozilla/5.0 (X11; " <> uname <> ") AppleWebKit/537 (KHTML, like Gecko) Chrome/25 Safari/537"
  , "Mozilla/5.0 (X11; " <> uname <> "; rv:21.0) Gecko/" <> today <> " Firefox/21.0"
  , "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)"
  ]
  where 
    uname = fromMaybe "unknown" $ Unsafe.unsafeDupablePerformIO $ captureLine "uname" ["-sm"]
    today = T.pack $ Time.formatTime Time.defaultTimeLocale "%Y%m%d" $ Time.utctDay $ Unsafe.unsafeDupablePerformIO Time.getCurrentTime

defaultSettings :: [G.AttrOp WK.Settings 'G.AttrConstruct]
defaultSettings =
  [ #enableCaretBrowsing              G.:= True
  , #enableDeveloperExtras            G.:= True
  , #enableFullscreen                 G.:= False
  , #enableHtml5Database              G.:= False
  , #enableHtml5LocalStorage          G.:= False
  , #enableJava                       G.:= False
  , #enableOfflineWebApplicationCache G.:= False
  , #enablePlugins                    G.:= False
  , #hardwareAccelerationPolicy       G.:= WK.HardwareAccelerationPolicyNever
  , #mediaPlaybackRequiresUserGesture G.:= True
  -- , #userAgent                        G.:= head userAgents
  , #zoomTextOnly                     G.:= True
  ]
