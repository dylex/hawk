module URI.Expand
  ( uriExpand
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Network.URI (escapeURIChar)
import           System.Directory (getHomeDirectory)

import Types
import Config

okInArg :: Char -> Bool
okInArg c = isAlphaNum c || c `elem` ("!$'()*,/:" :: String)

escapeURIChar' :: (Char -> Bool) -> Char -> String
escapeURIChar' p ' ' | not (p ' ') = "+"
escapeURIChar' p c = escapeURIChar p c

escapeURI :: (Char -> Bool) -> T.Text -> T.Text
escapeURI p = T.concatMap (T.pack . escapeURIChar' p)

uriExpand :: T.Text -> HawkM T.Text
uriExpand = expand . T.strip where
  expand s = case T.uncons s of
    Nothing -> return "about:blank"
    Just ('/',_) -> return $ "file://" <> s
    Just ('~',p) | T.isPrefixOf "/" p -> do
      h <- liftIO $ getHomeDirectory
      return $ "file://" <> T.pack h <> p
    _ -> case T.uncons r of
      Just (':',_) -> return s
      Just (' ',_) -> do
        rm <- asksConfig configURIRewrite
        return $ maybe
          (maybe
            s
            (rewrite s)
            $ HM.lookup T.empty rm)
          (rewrite r)
          $ HM.lookup a rm
      ~Nothing -> do
        am <- asksConfig configURIAlias
        return $ fromMaybe
          ("https://" <> s)
          $ HM.lookup s am
    where (a,r) = T.break (\c -> c == ' ' || c == ':') s
  rewrite arg pfx = pfx <> escapeURI okInArg arg

