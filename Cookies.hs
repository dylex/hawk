{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Cookies 
  ( emptyCookies
  , loadCookiesTxt
  , saveCookiesTxt
  , askCookieManager
  , addCookiesTo
  , addCookies
  , saveCookies
  ) where

#if 0
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif
import           Control.Monad ((<=<), guard, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Function (on)
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
#if MIN_VERSION_time(1,8,0)
import           Data.Time.Clock.System (systemSeconds, getSystemTime)
#else
import           Data.Time.Clock.POSIX (getPOSIXTime)
#endif

import qualified GI.Gio as Gio
import qualified GI.Soup as Soup
import qualified GI.WebKit2 as WK

import Types

data Cookie = Cookie
  { cookieDomain, cookiePath, cookieName, cookieValue :: !T.Text
  , cookieSecure :: !Bool
  , cookieHttpOnly :: !Bool
  , cookieExpires :: !Int
  , cookieMaxAge :: !Int
  } deriving (Eq, Show)

instance Ord Cookie where
  compare a b = mconcat $ map (\f -> on compare f a b) [T.reverse . cookieDomain, cookiePath, cookieName]

type Cookies = Set.Set Cookie

emptyCookies :: Cookies
emptyCookies = Set.empty

parseCookieTxt :: Int -> BS.ByteString -> Maybe Cookie
parseCookieTxt now = pc . BSC.split '\t' where
  pc [domain, httponly, path, secure, expire, name, value] = do
    sec <- tf secure
    ho <- tf httponly
    (expi, _) <- BSC.readInt expire
    let hoddom = BS.stripPrefix "#HttpOnly_" domain
    return Cookie
      { cookieDomain = TE.decodeUtf8 $ fromMaybe domain hoddom
      , cookiePath = TE.decodeUtf8 path
      , cookieName = TE.decodeUtf8 name
      , cookieValue = TE.decodeUtf8 value
      , cookieSecure = sec
      , cookieHttpOnly = ho || isJust hoddom
      , cookieExpires = expi
      , cookieMaxAge = expi - now
      }
  pc _ = Nothing
  tf "FALSE" = Just False
  tf "0" = Just False
  tf "TRUE" = Just True
  tf "1" = Just True
  tf _ = Nothing

mintersperse :: Monoid m => m -> [m] -> m
mintersperse _ [] = mempty
mintersperse d (x:l) = x <> mconcat (map (d <>) l)

writeCookieTxt :: Cookie -> BSB.Builder
writeCookieTxt Cookie{..} = mintersperse (BSB.char7 '\t')
  [ TE.encodeUtf8Builder cookieDomain
  , tf cookieHttpOnly -- INCOMPATIBLE: $ not (BS.null (cookieDomain c)) && BS.head (cookieDomain c) == '.'
  , TE.encodeUtf8Builder cookiePath
  , tf cookieSecure
  , BSB.intDec cookieExpires
  , TE.encodeUtf8Builder cookieName
  , TE.encodeUtf8Builder cookieValue
  ] where
  tf False = "FALSE"
  tf True = "TRUE"

checkExpired :: Cookie -> Maybe Cookie
checkExpired c = c <$ guard (cookieMaxAge c > 0)

getTime :: IO Int
getTime =
#if MIN_VERSION_time(1,8,0)
  systemSeconds <$> getSystemTime
#else
  round <$> getPOSIXTime
#endif

loadCookiesTxt :: FilePath -> IO Cookies
loadCookiesTxt f = do
  now <- getTime
  Set.fromDistinctAscList . mapMaybe (checkExpired <=< parseCookieTxt now) . BSC.lines <$> BS.readFile f

saveCookiesTxt :: FilePath -> Cookies -> IO ()
saveCookiesTxt f s =
  BSL.writeFile f $! BSB.toLazyByteString $ foldMap (foldMap (\c -> writeCookieTxt c <> BSB.char7 '\n') . checkExpired) s

newSoupCookie :: Cookie -> IO Soup.Cookie
newSoupCookie Cookie{..} = do
  c <- Soup.cookieNew cookieName cookieValue cookieDomain cookiePath (fromIntegral cookieMaxAge)
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
    , cookieMaxAge = -1 -- unused
    }

addCookie :: WK.CookieManager -> Cookie -> IO ()
addCookie cm c = do
  s <- newSoupCookie c
#if 0
  v <- newEmptyMVar
  WK.cookieManagerAddCookie cm s Gio.noCancellable $ Just $ \_ ->
    putMVar v <=< WK.cookieManagerAddCookieFinish cm
  takeMVar v
#else
  WK.cookieManagerAddCookie cm s Gio.noCancellable $ Just $ \_ cb -> do
    WK.cookieManagerAddCookieFinish cm cb
    {-
    print =<< getSoupCookie s
    print =<< Soup.cookieDomainMatches s "foo.com"
    print =<< Soup.cookieAppliesToUri s . fromJust =<< Soup.uRINew (Just "https://foo.com/")
    -}
#endif

askCookieManager :: HawkM WK.CookieManager
askCookieManager = #getCookieManager =<< asks (globalWebContext . hawkGlobal)

addCookiesTo :: WK.CookieManager -> Cookies -> IO ()
addCookiesTo = mapM_ . addCookie

addCookies :: Cookies -> HawkM ()
addCookies s = do
  cm <- askCookieManager
  liftIO $ addCookiesTo cm s

saveCookies :: T.Text -> HawkM ()
saveCookies uri = do
  cm <- askCookieManager
  #getCookies cm uri Gio.noCancellable $ Just $ \_ cb -> do
    cl <- #getCookiesFinish cm cb
    print =<< mapM getSoupCookie cl
