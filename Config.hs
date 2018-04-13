{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config
  ( GValue(..)
  , makeGValue
  , setObjectProperty
  , Config(..)
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as J (null_)
import qualified Data.Aeson.Types as J (typeMismatch, parseEither, emptyObject)
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import qualified Data.GI.Base as G
import qualified Data.GI.Base.GValue as GValue
import qualified Data.GI.Base.Properties as GProp
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid (Monoid(..))
import           Data.Scientific (floatingOrInteger)
import           Data.Semigroup (Semigroup(..))
import qualified Data.Text as T
import           Data.Word (Word32, Word16)
import           Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase)
import           Foreign.Ptr (nullPtr)
import           Network (PortID(..))
import           System.Environment (getEnv)
import           System.FilePath ((</>))
import qualified System.IO.Unsafe as Unsafe

import qualified GI.WebKit2 as WK

data GValue
  = GValueNull
  | GValueString T.Text
  | GValueInt Int64
  | GValueDouble Double
  | GValueBool Bool

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
makeGValue GValueNull       = GValue.buildGValue G.gtypePointer GValue.set_pointer nullPtr
makeGValue (GValueString x) = GValue.buildGValue G.gtypeString  GValue.set_string (Just x)
makeGValue (GValueInt    x) = GValue.buildGValue G.gtypeInt64   GValue.set_int64 x
makeGValue (GValueDouble x) = GValue.buildGValue G.gtypeDouble  GValue.set_double x
makeGValue (GValueBool   x) = GValue.buildGValue G.gtypeBoolean GValue.set_boolean x

setObjectProperty :: G.GObject a => a -> String -> GValue -> IO ()
setObjectProperty o p GValueNull        = GProp.setObjectPropertyPtr    o p nullPtr
setObjectProperty o p (GValueString x)  = GProp.setObjectPropertyString o p (Just x)
setObjectProperty o p (GValueInt x)     = GProp.setObjectPropertyInt64  o p x
setObjectProperty o p (GValueDouble x)  = GProp.setObjectPropertyDouble o p x
setObjectProperty o p (GValueBool x)    = GProp.setObjectPropertyBool   o p x

type GObjectConfig = HM.HashMap T.Text GValue

data Config = Config
  { configDatabase :: !(Maybe PGDatabase)
  , configUserAgents :: !(V.Vector T.Text)

  -- WebSettings
  , configSettings :: !GObjectConfig

  -- UserContentManager
  , configStyleSheet :: ![FilePath]
  , configScript :: ![FilePath]

  -- WebsiteDataManager
  , configDataDirectory :: !(Maybe FilePath)
  , configCookieFile :: !(Maybe FilePath)
  , configCookieAcceptPolicy :: !WK.CookieAcceptPolicy

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
  , configPrivateMode :: !Bool
  }

defaultParse :: J.FromJSON a => a
defaultParse = either error id $ J.parseEither J.parseJSON J.emptyObject

instance Default Config where
  def = defaultParse

instance Semigroup Config where
  a <> b = b
    { configSettings = HM.union (configSettings b) (configSettings a)
    , configStyleSheet = configStyleSheet a ++ configStyleSheet b
    , configScript = configScript a ++ configScript b
    , configDataDirectory = configDataDirectory b <|> configDataDirectory a
    , configCookieFile = configCookieFile b <|> configCookieFile a
    , configProxy = configProxy b <|> configProxy a
    , configProxyIgnore = configProxyIgnore a ++ configProxyIgnore b
    }

instance Monoid Config where
  mempty = def
  mappend = (<>)

newtype ListOr a = ListOr{ orList :: [a] }

instance J.FromJSON a => J.FromJSON (ListOr a) where
  parseJSON J.Null = return $ ListOr []
  parseJSON x@(J.Array _) = ListOr <$> J.parseJSON x
  parseJSON x = ListOr . return <$> J.parseJSON x

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
      }

instance Default PGDatabase where
  def = defaultParse

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

instance J.FromJSON Config where
  parseJSON = J.withObject "config" $ \c -> do
    -- TODO: extra fields
    let get' f = c J..:! f
        get f = c J..:? f
        list f = foldMap orList <$> get' f
    configDatabase            <- get' "database" J..!= Just def
    configSettings            <- get  "settings" J..!= HM.empty
    configStyleSheet          <- list "style-sheet"
    configScript              <- list "script"
    configDataDirectory       <- get' "data-directory"
    configCookieFile          <- get' "cookie-file" J..!= ((</> "cookies.txt") <$> configDataDirectory)
    configCookieAcceptPolicy  <- get' "cookie-accept-policy" J..!= maybe WK.CookieAcceptPolicyNever (const WK.CookieAcceptPolicyNoThirdParty) configCookieFile
    configCacheModel          <- get' "cache-model" J..!= WK.CacheModelWebBrowser
    configProcessCountLimit   <- get  "process-count-limit" J..!= 0
    configProxy               <- get  "proxy"
    configProxyIgnore         <- list "proxy-ignore"
    configSpellChecking       <- get' "spell-checking" J..!= Just True J..!= False
    configProcessModel        <- get' "process-model" J..!= WK.ProcessModelSharedSecondaryProcess
    configCharset             <- get  "charset"
    configEditable            <- get  "editable" J..!= False
    configZoomLevel           <- get  "zoom-level" J..!= 1
    configURI                 <- get  "uri"
    configPrivateMode         <- get  "private-mode" J..!= False
    return Config{..}
