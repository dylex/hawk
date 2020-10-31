{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config
  ( Config(..)
  , SiteConfig(..)
  , configAllowLoad'
  , configKeepHistory'
  , configCookieAcceptPolicy'
  , siteConfig
  , defaultSiteConfig
  , loadConfigFile
  , baseConfigFile
  , useTPGConfig
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow (left)
import           Control.Monad (MonadPlus, (<=<), mzero)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (Parser, typeMismatch, parseEither)
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import           Data.Foldable (fold)
import           Data.Function (on)
import qualified Data.GI.Base.Overloading as GO
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (isPrefixOf)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word16)
import qualified Data.Yaml as Y
import           Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase, useTPGDatabase)
import qualified Language.Haskell.TH as TH
import           Network.Socket (SockAddr(SockAddrUnix))
import           System.Environment (getEnv)
import           System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(XdgCache))
import           System.FilePath ((</>), (<.>), dropExtension, takeExtension, takeDirectory)
import           System.IO.Error (ioError, mkIOError, doesNotExistErrorType)
import qualified System.IO.Unsafe as Unsafe

import qualified GI.WebKit2 as WK

import JSON
import qualified Data.ListMap as LM
import qualified Data.PrefixMap as PM
import qualified Data.BitSet as ES
import Domain
import JS
import Util
import GValue
import GAttributes

type Settings = HM.HashMap String GValue

wkSettings :: HS.HashSet String
wkSettings = HS.fromList $ gAttributeList (Proxy :: Proxy (GO.AttributeList WK.Settings))

checkSettings :: MonadFail m => Settings -> m Settings
checkSettings s
  | True || HM.null d = return s
  | otherwise = fail $ "Unknown settings: " ++ show (HM.keys d)
  where d = HM.difference s (HS.toMap wkSettings)

data Config = Config
  { configDatabase :: !(Maybe PGDatabase)

  -- UserContentManager
  , configStyleSheet :: !(V.Vector FilePath)
  , configScript :: !(Maybe FilePath)

  -- WebsiteDataManager
  , configDataDirectory :: !(Maybe FilePath)
  , configCacheDirectory :: !(Maybe FilePath)
  , configCookieFile :: !(Maybe FilePath)

  -- WebContext
  , configCacheModel :: !WK.CacheModel
  , configProxy :: !(Maybe T.Text)
  , configProxyIgnore :: ![T.Text]
  , configSpellChecking :: !Bool
  , configProcessModel :: !WK.ProcessModel

  -- WebView
  , configCharset :: !(Maybe T.Text)
  , configEditable :: !Bool
  , configZoomLevel :: !Double
  , configURI :: !(Maybe T.Text)

  -- Hawk
  , configUserAgent :: !(V.Vector T.Text)
  , configTLSAccept :: !DomainPSet
  , configURIRewrite :: !(HM.HashMap T.Text T.Text)
  , configURIAlias :: !(HM.HashMap T.Text T.Text)
  , configDownloadDir :: !(Maybe FilePath)

  , configSite :: !(DomainMap SiteConfig)
  }

data SiteConfig = SiteConfig
  {
  -- WebSettings
    configSettings :: !Settings

  -- WebsiteDataManager
  , configITP :: !(Maybe Bool)
  , configCookieAcceptPolicy :: !(Maybe WK.CookieAcceptPolicy)

  -- Hawk
  , configKeepHistory :: !(Maybe Bool)
  , configAllowLoad :: !(DomainMap (ES.BitSet LoadElement))
  }

makeLenses' ''Config
makeLenses' ''SiteConfig

instance Default Config where
  def = Config
    { configDatabase = Just defaultParse
    , configUserAgent = V.empty
    , configStyleSheet = V.empty
    , configScript = Nothing
    , configDataDirectory = Nothing
    , configCacheDirectory = Just $ Unsafe.unsafeDupablePerformIO $ getXdgDirectory XdgCache "hawk"
    , configCookieFile = Nothing
    , configCacheModel = WK.CacheModelWebBrowser
    , configProxy = Nothing
    , configProxyIgnore = []
    , configSpellChecking = True
    , configProcessModel = WK.ProcessModelMultipleSecondaryProcesses
    , configCharset = Nothing
    , configEditable = False
    , configZoomLevel = 1
    , configURI = Nothing
    , configTLSAccept = PM.empty
    , configURIRewrite = HM.empty
    , configURIAlias = HM.empty
    , configDownloadDir = Nothing
    , configSite = LM.singleton [] def
    }

instance Default SiteConfig where
  def = SiteConfig
    { configSettings = HM.empty
    , configITP = Just True
    , configCookieAcceptPolicy = Just WK.CookieAcceptPolicyNever
    , configKeepHistory = Just True
    , configAllowLoad = LM.singleton [] ES.empty
    }

instance Semigroup SiteConfig where
  a <> b = SiteConfig
    { configSettings           = on HM.union configSettings           a b
    , configITP                = on (<|>)    configITP                a b
    , configCookieAcceptPolicy = on (<|>)    configCookieAcceptPolicy a b
    , configKeepHistory        = on (<|>)    configKeepHistory        a b
    , configAllowLoad          = on LM.union configAllowLoad          a b
    }

instance Monoid SiteConfig where
  mempty = SiteConfig
    { configSettings = HM.empty
    , configITP = Nothing
    , configCookieAcceptPolicy = Nothing
    , configKeepHistory = Nothing
    , configAllowLoad = LM.empty
    }
  mappend = (<>)

setSelf :: DomainComponents -> DomainMap a -> DomainMap a
setSelf d m =
  maybe id (ins d) (LM.lookup s m) $
  -- maybe id (ins (take 2 d)) (LM.lookup ss m)
  m
  where
  s = domainComponents "."
  -- ss = domainComponents ".."
  ins = LM.insertWith $ \_ -> id

setSelfSite :: DomainComponents -> SiteConfig -> SiteConfig
setSelfSite d c = c
  { configAllowLoad = setSelf d $ configAllowLoad c
  }

siteConfig :: Domain -> Config -> SiteConfig
siteConfig (Domain d) = setSelfSite d
  . LM.lookupFoldPrefixes d
  . configSite

defaultSiteConfig :: Config -> SiteConfig
defaultSiteConfig = siteConfig ""

instance J.FromJSON PGDatabase where
  parseJSON = J.withObject "database" $ \d -> do
    host  <- d J..:? "host"
    port  <- d J..:? "port"  J..!= (5432 :: Word16)
    sock  <- d J..:? "sock"  J..!= ("/tmp/.s.PGSQL." ++ show port)
    user  <- d J..:? "user"  J..!= Unsafe.unsafeDupablePerformIO (getEnv "USER")
    db    <- d J..:? "db"    J..!= user
    pass  <- d J..:? "pass"  J..!= ""
    debug <- d J..:? "debug" J..!= False
    return defaultPGDatabase
      { pgDBAddr = maybe (Right $ SockAddrUnix sock) (Left . (, show port)) host
      , pgDBName = BSC.pack db
      , pgDBUser = BSC.pack user
      , pgDBPass = BSC.pack pass
      , pgDBDebug = debug
      , pgDBParams = [("search_path", "hawk,global")]
      }

instance Default WK.CookieAcceptPolicy where
  def = WK.CookieAcceptPolicyNever

instance J.FromJSON WK.CookieAcceptPolicy where
  parseJSON J.Null                      = return WK.CookieAcceptPolicyNever
  parseJSON (J.Number 0)                = return WK.CookieAcceptPolicyNever
  parseJSON (J.Number 1)                = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.Number 2)                = return WK.CookieAcceptPolicyAlways
  parseJSON (J.Bool False)              = return WK.CookieAcceptPolicyNever
  parseJSON (J.Bool True)               = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.String "never")          = return WK.CookieAcceptPolicyNever
  parseJSON (J.String "Never")          = return WK.CookieAcceptPolicyNever
  parseJSON (J.String "no-third-party") = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.String "NoThirdParty")   = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.String "always")         = return WK.CookieAcceptPolicyAlways
  parseJSON (J.String "Always")         = return WK.CookieAcceptPolicyAlways
  parseJSON x = J.typeMismatch "CookieAcceptPolicy (never,no-third-party,always)" x

instance J.FromJSON WK.CacheModel where
  parseJSON J.Null                        = return WK.CacheModelDocumentViewer
  parseJSON (J.Number 0)                  = return WK.CacheModelDocumentViewer
  parseJSON (J.Bool False)                = return WK.CacheModelDocumentViewer
  parseJSON (J.String "document-viewer")  = return WK.CacheModelDocumentViewer
  parseJSON (J.String "DocumentViewer")   = return WK.CacheModelDocumentViewer
  parseJSON (J.String "none")             = return WK.CacheModelDocumentViewer
  parseJSON (J.Bool True)                 = return WK.CacheModelWebBrowser
  parseJSON (J.Number 2)                  = return WK.CacheModelWebBrowser
  parseJSON (J.String "web-browser")      = return WK.CacheModelWebBrowser
  parseJSON (J.String "WebBrowser")       = return WK.CacheModelWebBrowser
  parseJSON (J.String "web")              = return WK.CacheModelWebBrowser
  parseJSON (J.String "all")              = return WK.CacheModelWebBrowser
  parseJSON (J.Number 1)                  = return WK.CacheModelDocumentBrowser
  parseJSON (J.String "document-browser") = return WK.CacheModelDocumentBrowser
  parseJSON (J.String "DocumentBrowser")  = return WK.CacheModelDocumentBrowser
  parseJSON (J.String "some")             = return WK.CacheModelDocumentBrowser
  parseJSON x = J.typeMismatch "CacheModel (none,some,all)" x

instance J.FromJSON WK.ProcessModel where
  parseJSON J.Null                = return WK.ProcessModelSharedSecondaryProcess
  parseJSON (J.Bool False)        = return WK.ProcessModelSharedSecondaryProcess
  parseJSON (J.Bool True)         = return WK.ProcessModelMultipleSecondaryProcesses
  parseJSON (J.String "shared")   = return WK.ProcessModelSharedSecondaryProcess
  parseJSON (J.String "multiple") = return WK.ProcessModelMultipleSecondaryProcesses
  parseJSON x = J.typeMismatch "ProcessModel (shared,multiple)" x

newtype Some m a = Some{ getSome :: m a }

instance (MonadPlus m, J.FromJSON1 m, J.FromJSON a) => J.FromJSON (Some m a) where
  parseJSON J.Null = return $ Some mzero
  parseJSON x@(J.Array _) = Some <$> J.parseJSON1 x
  parseJSON x = Some . return <$> parseJSON x

parseSome :: (MonadPlus m, J.FromJSON1 m, J.FromJSON a) => J.Value -> J.Parser (m a)
parseSome v = getSome <$> parseJSON v

parserSiteConfig :: ObjectParser SiteConfig
parserSiteConfig = do
  configSettings'           .<~ "settings"  $ \s -> fmap (`HM.union` s) . checkSettings <=< parseJSON
  configITP'                .<- "itp"
  configCookieAcceptPolicy' .<- "cookie-accept-policy"
  configKeepHistory'        .<- "keep-history"
  configAllowLoad'          .<~ "allow-load" $ \s -> fmap (`LM.union` s) . parseJSON

instance J.FromJSON SiteConfig where
  parseJSON = parseObject mempty "site config" parserSiteConfig

parseConfig :: Config -> FilePath -> J.Value -> J.Parser Config
parseConfig initconf conffile = parseObject initconf "config" $ do
  configDatabase'           .<- "database"
  configStyleSheet'         .<~ "style-sheet" $ const $ fmap (fmap dir) . parseSome
  configScript'             .<~ "script"      $ const $ fmap (fmap dir) . parseSome
  configDataDirectory'      .<~ "data-directory" $ const $ \case
    J.Bool False -> return Nothing
    J.Bool True  -> return $ Just $ dropExtension conffile
    x -> parsePath x
  configCacheDirectory'     .<~ "cache-directory" $ const parsePath
  modifyObject $ \c -> c{ configCookieFile = (</> "cookies.txt") <$> configDataDirectory c }
  configCookieFile'         .<~ "cookie-file"     $ const parsePath
  -- modifyObject $ \c -> c{ configCookieAcceptPolicy = maybe WK.CookieAcceptPolicyNever (const WK.CookieAcceptPolicyNoThirdParty) $ configCookieFile c }
  configCacheModel'         .<- "cache-model"
  configProxy'              .<- "proxy"
  configProxyIgnore'        .<- "proxy-ignore"
  configSpellChecking'      .<- "spell-checking"
  configProcessModel'       .<- "process-model"
  configCharset'            .<- "charset"
  configEditable'           .<- "editable"
  configZoomLevel'          .<- "zoom-level"
  configURI'                .<- "uri"
  configUserAgent'          .<~ "user-agent" $ const parseSome
  configTLSAccept'          .<- "tls-accept"
  configURIRewrite'         .<~ "uri-rewrite" $ mergeWith . flip HM.union
  configURIAlias'           .<~ "uri-alias"   $ mergeWith . flip HM.union
  configDownloadDir'        .<- "download-dir"
  site <- parseSubObject (fold $ LM.lookup [] $ configSite initconf) parserSiteConfig
  configSite'               .<~ "site"        $ mergeWith . flip LM.union -- not: LM.unionWith mappend
  modifyObject $ \c -> c{ configSite = LM.insertWith mappend [] site $ configSite c }
  where
  dir = (takeDirectory conffile </>)
  parsePath = fmap (fmap dir) . parseJSON
  mergeWith f = fmap f . parseJSON

instance J.FromJSON Config where
  parseJSON = parseConfig def ""

loadConfigFile :: Config -> FilePath -> IO Config
loadConfigFile initconf conffile =
  either fail return
    . (J.parseEither (parseConfig initconf conffile) =<<)
    =<< case takeExtension conffile of
      ".json" -> json conffile
      ".yaml" -> yaml conffile
      ".yml"  -> yaml conffile
      _ -> ife yaml (conffile <.> "yaml")
         $ ife json (conffile <.> "json")
         $ yaml conffile
  where
  json f = J.eitherDecodeStrict <$> BSC.readFile f
  yaml f = yamle f =<< Y.decodeFileEither f
  yamle f (Left (Y.InvalidYaml (Just (Y.YamlException e))))
    | "Yaml file not found: " `isPrefixOf` e = ioError $ mkIOError doesNotExistErrorType e Nothing (Just f)
  yamle _ r = return $ left show r
  ife l f o = do
    e <- doesFileExist f
    if e then l f else o

baseConfigFile :: FilePath
baseConfigFile = "config"

useTPGConfig :: TH.DecsQ
useTPGConfig = maybe (return []) useTPGDatabase . configDatabase =<< TH.runIO (loadConfigFile def baseConfigFile)
