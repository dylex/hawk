{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config
  ( GValue(..)
  , makeGValue
  , setObjectProperty
  , Config(..)
  , loadConfigFile
  , baseConfigFile
  , useTPGConfig
  ) where

import           Control.Arrow (left)
import           Control.Monad (MonadPlus, mzero)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as J (null_)
import qualified Data.Aeson.Types as J (Parser, typeMismatch, parseEither)
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import qualified Data.GI.Base as G
import qualified Data.GI.Base.GValue as GValue
import qualified Data.GI.Base.Properties as GProp
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32, Word16)
import qualified Data.Yaml as Y
import           Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase, useTPGDatabase)
import           Foreign.Ptr (nullPtr)
import qualified Language.Haskell.TH as TH
import           Network (PortID(..))
import           System.Environment (getEnv)
import           System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(XdgCache))
import           System.FilePath ((</>), (<.>), dropExtension, takeExtension, takeDirectory)
import           System.IO.Error (ioError, mkIOError, doesNotExistErrorType)
import qualified System.IO.Unsafe as Unsafe

import qualified GI.WebKit2 as WK

import JSON
import qualified URI.ListMap as LM
import qualified URI.PrefixMap as PM
import URI.Domain (DomainPSet, DomainMap)
import Util

data GValue
  = GValueNull
  | GValueString T.Text
  | GValueInt Int64
  | GValueDouble Double
  | GValueBool Bool
  deriving (Eq, Show)

instance J.ToJSON GValue where
  toJSON GValueNull       = J.Null
  toJSON (GValueString x) = J.toJSON x
  toJSON (GValueInt x)    = J.toJSON x
  toJSON (GValueDouble x) = J.toJSON x
  toJSON (GValueBool x)   = J.toJSON x
  toEncoding GValueNull       = J.null_
  toEncoding (GValueString x) = J.toEncoding x
  toEncoding (GValueInt x)    = J.toEncoding x
  toEncoding (GValueDouble x) = J.toEncoding x
  toEncoding (GValueBool x)   = J.toEncoding x

instance J.FromJSON GValue where
  parseJSON J.Null = return GValueNull
  parseJSON (J.String x) = return $ GValueString x
  parseJSON (J.Number x) = return $ either GValueDouble GValueInt $ floatingOrInteger x
  parseJSON (J.Bool x) = return $ GValueBool x
  parseJSON x = J.typeMismatch "GValue" x

makeGValue :: GValue -> IO G.GValue
makeGValue GValueNull       = GValue.toGValue nullPtr
makeGValue (GValueString x) = GValue.toGValue (Just x)
makeGValue (GValueInt    x) = GValue.toGValue x
makeGValue (GValueDouble x) = GValue.toGValue x
makeGValue (GValueBool   x) = GValue.toGValue x

setObjectProperty :: G.GObject a => a -> String -> GValue -> IO ()
setObjectProperty o p GValueNull        = GProp.setObjectPropertyPtr    o p nullPtr
setObjectProperty o p (GValueString x)  = GProp.setObjectPropertyString o p (Just x)
setObjectProperty o p (GValueInt x)     = GProp.setObjectPropertyInt64  o p x
setObjectProperty o p (GValueDouble x)  = GProp.setObjectPropertyDouble o p x
setObjectProperty o p (GValueBool x)    = GProp.setObjectPropertyBool   o p x

type GObjectConfig = HM.HashMap String GValue

data Config = Config
  { configDatabase :: !(Maybe PGDatabase)

  -- WebSettings
  , configSettings :: !GObjectConfig

  -- UserContentManager
  , configStyleSheet :: !(V.Vector FilePath)
  , configScript :: !(V.Vector FilePath)

  -- WebsiteDataManager
  , configDataDirectory :: !(Maybe FilePath)
  , configCacheDirectory :: !(Maybe FilePath)
  , configCookieFile :: !(Maybe FilePath)
  , configCookieAcceptPolicy :: !(DomainMap WK.CookieAcceptPolicy)

  -- WebContext
  , configCacheModel :: !WK.CacheModel
  , configProcessCountLimit :: !Word32
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
  , configPrivateMode :: !Bool
  , configBlockLoad :: !(V.Vector T.Text)
  , configBlockLoadSrc, configAllowLoadSrc :: !DomainPSet
  , configTLSAccept :: !DomainPSet
  , configURIRewrite :: !(HM.HashMap T.Text T.Text)
  , configURIAlias :: !(HM.HashMap T.Text T.Text)
  }

makeLenses' ''Config

instance Default Config where
  def = Config
    { configDatabase = Just defaultParse
    , configUserAgent = V.empty
    , configSettings = HM.empty
    , configStyleSheet = V.empty
    , configScript = V.empty
    , configDataDirectory = Nothing
    , configCacheDirectory = Just $ Unsafe.unsafeDupablePerformIO $ getXdgDirectory XdgCache "hawk"
    , configCookieFile = Nothing
    , configCookieAcceptPolicy = LM.empty
    , configCacheModel = WK.CacheModelWebBrowser
    , configProcessCountLimit = 0
    , configProxy = Nothing
    , configProxyIgnore = []
    , configSpellChecking = True
    , configProcessModel = WK.ProcessModelSharedSecondaryProcess
    , configCharset = Nothing
    , configEditable = False
    , configZoomLevel = 1
    , configURI = Nothing
    , configPrivateMode = False
    , configBlockLoad = V.empty
    , configBlockLoadSrc = PM.empty
    , configAllowLoadSrc = PM.empty
    , configTLSAccept = PM.empty
    , configURIRewrite = HM.empty
    , configURIAlias = HM.empty
    }

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
      { pgDBHost = fromMaybe "localhost" host
      , pgDBPort = if isJust host
        then PortNumber (fromIntegral port)
        else UnixSocket sock
      , pgDBName = BSC.pack db
      , pgDBUser = BSC.pack user
      , pgDBPass = BSC.pack pass
      , pgDBDebug = debug
      , pgDBParams = [("search_path", "hawk,global")]
      }

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

parseConfig :: Config -> FilePath -> J.Value -> J.Parser Config
parseConfig initconf conffile = parseObject initconf "config" $ do
  configDatabase'           .<- "database"
  configSettings'           .<~ "settings" $ \s -> fmap (`HM.union` s) . parseJSON
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
  configCookieAcceptPolicy' .<- "cookie-accept-policy"
  configCacheModel'         .<- "cache-model"
  configProcessCountLimit'  .<- "process-count-limit"
  configProxy'              .<- "proxy"
  configProxyIgnore'        .<- "proxy-ignore"
  configSpellChecking'      .<- "spell-checking"
  configProcessModel'       .<- "process-model"
  configCharset'            .<- "charset"
  configEditable'           .<- "editable"
  configZoomLevel'          .<- "zoom-level"
  configURI'                .<- "uri"
  configUserAgent'          .<~ "user-agent" $ const parseSome
  configPrivateMode'        .<- "private-mode"
  configBlockLoad'          .<- "block-load"
  configBlockLoadSrc'       .<- "block-load-src"
  configAllowLoadSrc'       .<- "allow-load-src"
  configTLSAccept'          .<- "tls-accept"
  configURIRewrite'         .<- "uri-rewrite"
  configURIAlias'           .<- "uri-alias"
  where
  dir = (takeDirectory conffile </>)
  parsePath = fmap (fmap dir) . parseJSON

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
