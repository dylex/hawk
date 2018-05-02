{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Event
  ( loadStarted
  , loadFinished
  , targetChanged
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Bits ((.&.))
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
import URI
import URI.Domain
import qualified URI.ListMap as LM

useTPGConfig

loadStarted :: HawkM ()
loadStarted = do
  wv <- asks hawkWebView
  uri <- G.get wv #uri
  cm <- askCookieManager
  conf <- asksConfig configCookieAcceptPolicy
  let dom = uriDomain =<< uri
      pol = LM.lookupPrefix (maybe [] domainComponents dom) conf
  liftIO $ print (dom, pol)
  mapM_ (#setAcceptPolicy cm) pol

loadFinished :: HawkM ()
loadFinished = do
  p <- readRef hawkPrivateMode
  unless p $ do
    wv <- asks hawkWebView
    uri <- G.get wv #uri
    when (any (T.isPrefixOf "http") uri) $ do
      title <- G.get wv #title
      hawkDBExecute $ (\(_ :: Maybe Int32) -> ()) <$> [pgSQL|$SELECT browse_add(${uri}, ${title})|]

targetChanged :: WK.HitTestResult -> Word32 -> HawkM ()
targetChanged targ _ = do
  b <- readRef hawkBindings
  when (isCommand b) $ do
    ctx <- G.get targ #context
    stat <- asks hawkStatusLeft
    let chk t = ctx .&. fromIntegral (fromEnum t) /= 0
        set :: (KnownSymbol attr, GA.AttrGetC info WK.HitTestResult attr T.Text) => GA.AttrLabelProxy attr -> HawkM ()
        set p = #setText stat =<< G.get targ p
    if
      | chk WK.HitTestResultContextLink  -> set #linkUri
      | chk WK.HitTestResultContextImage -> set #imageUri
      | chk WK.HitTestResultContextMedia -> set #mediaUri
      | otherwise -> return ()
  where
  isCommand Command{} = True
  isCommand _ = False
