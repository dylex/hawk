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
  , reapplySiteConfig
  , startDownload
  ) where

import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Bits ((.&.))
import           Data.Char (isAlphaNum)
import           Data.Foldable (fold)
import qualified Data.GI.Base as G
import qualified Data.GI.Base.Attributes as GA
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int32)
import qualified Data.Text as T
import           Data.Word (Word32)
import           Database.PostgreSQL.Typed (pgSQL)
import           GHC.TypeLits (KnownSymbol)
import           System.Directory (doesPathExist)
import           System.FilePath ((</>), (<.>))

import qualified GI.WebKit as WK

import Types
import Config
import Database
import URI
import Domain
import GValue
import UI

useTPGConfig

siteConfigFor :: Domain -> HawkM SiteConfig
siteConfigFor dom = mappend <$> readRef hawkSiteOverride <*> asksConfig (siteConfig dom)

askSiteConfig :: HawkM SiteConfig
askSiteConfig = siteConfigFor =<< readRef hawkURIDomain

applySiteConfig :: SiteConfig -> HawkM ()
applySiteConfig conf = do
  settings <- askSettings
  liftIO $ mapM_ (uncurry $ setObjectProperty settings) $ HM.toList $ configSettings conf
  -- dm <- askWebsiteDataManager
  -- mapM_ (#setItpEnabled dm) $ configITP conf
  cm <- askCookieManager
  mapM_ (#setAcceptPolicy cm) $ configCookieAcceptPolicy conf

reapplySiteConfig :: HawkM ()
reapplySiteConfig = applySiteConfig =<< askSiteConfig

uriChanged :: Maybe T.Text -> HawkM ()
uriChanged uri = do
  dch <- modifyRef hawkURIDomain $ (,) dom . (dom /=)
  when dch $ applySiteConfig =<< siteConfigFor dom
  where
  dom = fold $ uriDomain =<< uri

loadStarted :: HawkM ()
loadStarted = do
  commandModeBind

loadCommitted :: HawkM ()
loadCommitted = do
  return ()

loadFinished :: HawkM ()
loadFinished = do
  conf <- askSiteConfig
  when (or $ configKeepHistory conf) $ do
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

startDownload :: WK.Download -> HawkM ()
startDownload dl = do
  stat <- asks hawkStatusLeft
  let dest f = do
        _ <- G.on dl #failed $ \err -> do
          msg <- ((f <> ": ") <>) <$> G.gerrorMessage err
          #setText stat msg
          print msg
        _ <- G.on dl #createdDestination $ #setText stat
        #setDestination dl ("file://" <> f)
  maybe (#cancel dl) (\dir -> void $ G.on dl #decideDestination $ \fn -> do
    let fn' = nonempt $ T.unpack $ T.map sani $ T.takeWhileEnd ('/' /=) fn
    maybe (#cancel dl) (dest . T.pack) =<< choose (dir </> fn') 0
    return True)
    =<< asksConfig configDownloadDir
  where
  sani c
    | isAlphaNum c || c `elem` ("()+,-.:@" :: [Char]) = c
    | otherwise = '_'
  nonempt "" = "_"
  nonempt f = f
  ext f 0 = f
  ext f i = f <.> show (i :: Int)
  choose _ 10 = return Nothing
  choose f i = do
    e <- doesPathExist f'
    if e then choose f (succ i) else return $ Just f'
    where f' = ext f i
