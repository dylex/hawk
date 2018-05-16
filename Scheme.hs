{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Scheme
  ( hawkURIScheme
  ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Data.IORef (newIORef, modifyIORef', readIORef)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (utcToLocalZonedTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Database.PostgreSQL.Typed (pgSQL)
import qualified Text.Blaze.Html.Renderer.Utf8 as HU
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified System.IO.Unsafe as Unsafe

import qualified GI.WebKit2 as WK
import qualified GI.Gio as Gio

import Types
import Config
import Database
import URI ()

useTPGConfig

respondHtml :: WK.URISchemeRequest -> H.Html -> HawkM ()
respondHtml req h = liftIO $ do
  is <- Gio.memoryInputStreamNew
  lr <- newIORef 0
  HU.renderHtmlToByteStringIO (\b -> do
    modifyIORef' lr (fromIntegral (BS.length b) +)
    Gio.memoryInputStreamAddData is b Nothing)
    $ H.docTypeHtml h
  len <- readIORef lr
  #finish req is len (Just "text/html;charset=utf-8")

listBrowse :: [(T.Text, Maybe T.Text, Maybe UTCTime)] -> H.Html
listBrowse b = H.table $ do
  H.tbody $
    forM_ b $ \(u, t, l) ->
      H.tr $ do
        H.td H.! HA.style "white-space:nowrap" $ mapM_ (H.string . formatTime defaultTimeLocale "%F %T" . Unsafe.unsafeDupablePerformIO . utcToLocalZonedTime) l
        H.td $ H.a H.! HA.href (H.textValue u) $ H.text (fromMaybe u t)

hawkURIScheme :: WK.URISchemeRequest -> HawkM ()
hawkURIScheme req = do
  path <- #getPath req
  h <- case path of
    "marks" -> (H.head (H.title "Marks") <>) . H.body . listBrowse <$>
      hawkDBQuery [pgSQL|!SELECT COALESCE(browse.uri, mark.uri), NULLIF(browse.title, ''), browse.last
         FROM mark LEFT JOIN browse ON (mark.browse = browse.id)
        ORDER BY last DESC NULLS LAST|]
    "history" -> (H.head (H.title "Favorite history") <>) . H.body . listBrowse <$>
      hawkDBQuery [pgSQL|SELECT uri, NULLIF(title, ''), last FROM
        (SELECT uri, title, last, row_number() OVER d AS i
           FROM browse
         WINDOW d AS (PARTITION BY (uri).domain ORDER BY last DESC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)) b
          WHERE i = 1 ORDER BY last DESC NULLS LAST|]
    _ -> return $ "404: " <> H.text path
  respondHtml req h
