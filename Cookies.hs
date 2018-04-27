{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cookies 
  ( loadCookies
  , saveCookies
  ) where

import           Control.Monad ((<=<), guard, when, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Function (on)
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
#if MIN_VERSION_time(1,8,0)
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
#else
import           Data.Time.Clock.POSIX (getPOSIXTime)
#endif
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import           Database.PostgreSQL.Typed (PGConnection, pgSQL, pgQuery, pgExecute)

import qualified GI.Gio as Gio
import qualified GI.Soup as Soup
import qualified GI.WebKit2 as WK

import Util
import Types
import Config

useTPGConfig

data Cookie = Cookie
  { cookieDomain, cookiePath, cookieName, cookieValue :: !T.Text
  , cookieSecure :: !Bool
  , cookieHttpOnly :: !Bool
  , cookieExpires :: !Int
  } deriving (Eq, Show)

instance Ord Cookie where
  compare a b = mconcat $ map (\f -> on compare f a b) [cookieDomain, cookiePath, cookieName]

parseCookieTxt :: BS.ByteString -> Maybe Cookie
parseCookieTxt = pc . BSC.split '\t' where
  pc [domain, flag, path, secure, expire, name, value] = do
    sec <- tf secure
    _flag <- tf flag
    (expi, _) <- BSC.readInt expire
    let hoddom = BS.stripPrefix "#HttpOnly_" domain
    return Cookie
      { cookieDomain = TE.decodeUtf8 $ fromMaybe domain hoddom
      , cookiePath = TE.decodeUtf8 path
      , cookieName = TE.decodeUtf8 name
      , cookieValue = TE.decodeUtf8 value
      , cookieSecure = sec
      , cookieHttpOnly = isJust hoddom
      , cookieExpires = expi
      }
  pc _ = Nothing
  tf "FALSE" = Just False
  tf "TRUE" = Just True
  tf _ = Nothing

writeCookieTxt :: Cookie -> BSB.Builder
writeCookieTxt Cookie{..} = mintersperse (BSB.char7 '\t')
  [ (if cookieHttpOnly then "#HttpOnly_" else mempty) <> TE.encodeUtf8Builder cookieDomain
  , tf $ "." `T.isPrefixOf` cookieDomain
  , TE.encodeUtf8Builder cookiePath
  , tf cookieSecure
  , BSB.intDec cookieExpires
  , TE.encodeUtf8Builder cookieName
  , TE.encodeUtf8Builder cookieValue
  ] where
  tf False = "FALSE"
  tf True = "TRUE"

checkExpired :: Int -> Cookie -> Maybe Cookie
checkExpired t c = c <$ guard (cookieExpires c > t)

getTime :: IO Int
getTime =
#if MIN_VERSION_time(1,8,0)
  systemSeconds <$> getSystemTime
#else
  round <$> getPOSIXTime
#endif

loadCookiesTxt :: FilePath -> IO [Cookie]
loadCookiesTxt f = do
  t <- getTime
  foldMap (mapMaybe (checkExpired t <=< parseCookieTxt) . BSC.lines)
    <$> fromDoesNotExist Nothing (Just <$> BS.readFile f)

saveCookiesTxt :: FilePath -> [Cookie] -> IO ()
saveCookiesTxt f s = do
  -- TODO: merge with existing cookies
  t <- getTime
  BSL.writeFile f $ BSB.toLazyByteString $ foldMap (foldMap (\c -> writeCookieTxt c <> BSB.char7 '\n') . checkExpired t) s

loadCookiesDB :: PGConnection -> IO [Cookie]
loadCookiesDB db = do
  -- might as well expire them now
  _ <- pgExecute db [pgSQL|DELETE FROM cookie WHERE expires <= now()|]
  pgQuery db (toCookie <$> [pgSQL|!SELECT domain::text, path, name, value, secure, httponly, expires FROM cookie|])
  where
  toCookie (cookieDomain, cookiePath, cookieName, cookieValue, cookieSecure, cookieHttpOnly, expires) = Cookie{ cookieExpires = maybe (-1) (round . utcTimeToPOSIXSeconds) expires, ..}

saveCookieDB :: PGConnection -> Cookie -> IO ()
saveCookieDB pg Cookie{..} =
  void $ pgExecute pg [pgSQL|INSERT INTO
    cookie (domain, path, name, value, secure, httponly, expires)
    VALUES (${cookieDomain}::text::domainname, ${cookiePath}, ${cookieName}, ${cookieValue}, ${cookieSecure}, ${cookieHttpOnly}, ${posixSecondsToUTCTime $ fromIntegral cookieExpires})
    ON CONFLICT (domain, path, name) DO UPDATE SET value = excluded.value, secure = excluded.secure, httponly = excluded.httponly, expires = excluded.expires
  |]

saveCookiesDB :: PGConnection -> [Cookie] -> IO ()
saveCookiesDB pg s = mapM_ (saveCookieDB pg) s

newSoupCookie :: Int -> Cookie -> IO Soup.Cookie
newSoupCookie t Cookie{..} = do
  c <- Soup.cookieNew cookieName cookieValue cookieDomain cookiePath (fromIntegral $ cookieExpires - t)
  Soup.cookieSetExpires c =<< Soup.dateNewFromTimeT (fromIntegral cookieExpires)
  when cookieSecure   $ Soup.cookieSetSecure   c cookieSecure
  when cookieHttpOnly $ Soup.cookieSetHttpOnly c cookieHttpOnly
  return c

getSoupCookie :: Soup.Cookie -> IO Cookie
getSoupCookie s = do
  domain   <- Soup.cookieGetDomain s
  path     <- Soup.cookieGetPath s
  name     <- Soup.cookieGetName s
  value    <- Soup.cookieGetValue s
  secure   <- Soup.cookieGetSecure s
  httponly <- Soup.cookieGetHttpOnly s
  expires  <- (maybe (return 0) Soup.dateToTimeT =<< Soup.cookieGetExpires s)
  return Cookie
    { cookieDomain = domain
    , cookiePath = path
    , cookieName = name
    , cookieValue = value
    , cookieSecure = secure
    , cookieHttpOnly = httponly
    , cookieExpires = fromIntegral expires
    }

addCookie :: WK.CookieManager -> Int -> Cookie -> IO ()
addCookie cm t c = do
  s <- newSoupCookie t c
  WK.cookieManagerAddCookie cm s Gio.noCancellable $ Just $ \_ cb -> do
    WK.cookieManagerAddCookieFinish cm cb

addCookies :: [Cookie] -> HawkM ()
addCookies s = do
  cm <- askCookieManager
  liftIO $ do
    t <- getTime
    mapM_ (addCookie cm t) s

getCookies :: T.Text -> ([Cookie] -> IO ()) -> HawkM ()
getCookies uri f = do
  cm <- askCookieManager
  #getCookies cm uri Gio.noCancellable $ Just $ \_ cb ->
    f =<< mapM getSoupCookie =<< #getCookiesFinish cm cb

cookieStore :: HawkM (Maybe (Either PGConnection FilePath))
cookieStore =
  maybe
    (fmap Left <$> asks hawkDatabase)
    (const $ return Nothing) -- (return . Right)
    =<< asks (configCookieFile . hawkConfig)

loadCookies :: HawkM ()
loadCookies =
  mapM_ (addCookies <=< liftIO . either loadCookiesDB loadCookiesTxt)
    =<< cookieStore

saveCookies :: T.Text -> HawkM ()
saveCookies uri =
  -- TODO: remove old cookies for site (if db)
  mapM_ (\store ->
    getCookies uri (either saveCookiesDB saveCookiesTxt store))
    =<< cookieStore
