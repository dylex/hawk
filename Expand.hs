module Expand
  ( expandURIWith
  , expandURI
  ) where

import           Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           System.Directory (getHomeDirectory)
import qualified System.IO.Unsafe as Unsafe

import Util
import Types
import Config
import URI

homeDir :: T.Text
homeDir = T.pack $ Unsafe.unsafeDupablePerformIO getHomeDirectory

okInArg :: Char -> Bool
okInArg c = isAlphaNum c || c `elem` ("!$'()*,/:" :: String)

expandURIWith :: Config -> T.Text -> T.Text
expandURIWith conf = expand . T.strip where
  expand s = case T.uncons s of
    Nothing -> "about:blank"
    Just ('/',_) -> "file://" <> s
    Just ('~',p) | T.isPrefixOf "/" p ->
      "file://" <> homeDir <> p
    _ -> case T.uncons r of
      Just (':',_) -> s
      Just (' ',_) ->
        maybe
          (mayrw $ HM.lookup T.empty rm)
          (rewrite r)
          $ HM.lookup a rm
      ~Nothing ->
        fromMaybe
          (mayrw $ if T.any ('.' ==) s
            then Nothing
            else HM.lookup T.empty (configURIRewrite conf))
          $ HM.lookup s am
    where
    (a,r) = T.break (\c -> c == ' ' || c == ':') s
    rewrite arg pfx = pfx <> escapeURI okInArg arg
    mayrw = maybe ("https://" <> s) (rewrite s)
  rm = configURIRewrite conf
  am = configURIAlias conf

expandURI :: T.Text -> HawkM T.Text
expandURI = app $ asksConfig expandURIWith
