{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Event
  ( uriChanged
  , loadStarted
  , loadCommitted
  , loadFinished
  , targetChanged
  , applySiteConfig
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Bits ((.&.))
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as GA
import           Data.Int (Int32)
import qualified Data.Text as T
import           Data.Word (Word32)
import           Database.PostgreSQL.Typed (pgSQL)
import           GHC.TypeLits (KnownSymbol)

import qualified GI.WebKit2 as WK

import Types
import Config
import Database
import Script
import URI
import Domain

useTPGConfig

applySiteConfigFor :: Domain -> HawkM ()
applySiteConfigFor dom = do
  conf <- mappend <$> readRef hawkSiteOverride <*> asksConfig (siteConfig dom)
  let pol = configCookieAcceptPolicy conf
  liftIO $ print pol
  cm <- askCookieManager
  mapM_ (#setAcceptPolicy cm) pol
  loadScripts conf

applySiteConfig :: HawkM ()
applySiteConfig = applySiteConfigFor =<< readRef hawkURIDomain

uriChanged :: Maybe T.Text -> HawkM ()
uriChanged uri = do
  liftIO $ print uri
  dch <- modifyRef hawkURIDomain $ (,) dom . (dom /=)
  when dch $ applySiteConfigFor dom
  where
  dom = fold $ uriDomain =<< uri

loadStarted :: HawkM ()
loadStarted = do
  return ()

loadCommitted :: HawkM ()
loadCommitted = do
  return ()

loadFinished :: HawkM ()
loadFinished = do
  p <- readRef hawkPrivateMode
  unless p $ do
    wv <- askWebView
    uri <- #getUri wv
    when (any (T.isPrefixOf "http") uri) $ do
      title <- #getTitle wv
      hawkDBExecute $ (\(_ :: Maybe Int32) -> ()) <$> [pgSQL|$SELECT browse_add(${uri}, ${title})|]

targetChanged :: WK.HitTestResult -> Word32 -> HawkM ()
targetChanged targ _ = do
  b <- readRef hawkBindings
  when (isCommand b) $ do
    ctx <- #getContext targ
    stat <- asks hawkStatusLeft
    let chk t = ctx .&. fromIntegral (fromEnum t) /= 0
        set :: (KnownSymbol attr, GA.AttrGetC info WK.HitTestResult attr (Maybe T.Text)) => GA.AttrLabelProxy attr -> HawkM ()
        set p = #setText stat . fold =<< G.get targ p
    if
      | chk WK.HitTestResultContextLink  -> set #linkUri
      | chk WK.HitTestResultContextImage -> set #imageUri
      | chk WK.HitTestResultContextMedia -> set #mediaUri
      | otherwise -> return ()
  where
  isCommand Command{} = True
  isCommand _ = False
