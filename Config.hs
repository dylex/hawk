module Config
  ( defaultDatabase
  , GValue(..)
  , makeGValue
  , Config(..)
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as J (null_)
import qualified Data.Aeson.Types as J (typeMismatch, parseEither, emptyObject)
import           Data.Default (Default(def))
import qualified Data.GI.Base as G
import qualified Data.GI.Base.GValue as GValue
import qualified Data.HashMap.Strict as HM
import           Data.Monoid (Monoid(..))
import           Data.Scientific (floatingOrInteger)
import           Data.Semigroup (Semigroup(..))
import qualified Data.Text as T
import           Data.Word (Word32)
import           Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase)
import           Foreign.Ptr (nullPtr)
import           Network (PortID(..))

import qualified GI.WebKit2 as WK

defaultDatabase :: PGDatabase
defaultDatabase = defaultPGDatabase
  { pgDBUser = "dylan"
  , pgDBName = "dylan"
  , pgDBPort = UnixSocket "/tmp/.s.PGSQL.5432"
  }

data GValue
  = GValueNull
  | GValueString T.Text
  | GValueInt Int
  | GValueDouble Double
  | GValueBool Bool

instance J.ToJSON GValue where
  toJSON GValueNull = J.Null
  toJSON (GValueString x) = J.toJSON x
  toJSON (GValueInt x) = J.toJSON x
  toJSON (GValueDouble x) = J.toJSON x
  toJSON (GValueBool x) = J.toJSON x
  toEncoding GValueNull = J.null_
  toEncoding (GValueString x) = J.toEncoding x
  toEncoding (GValueInt x) = J.toEncoding x
  toEncoding (GValueDouble x) = J.toEncoding x
  toEncoding (GValueBool x) = J.toEncoding x

instance J.FromJSON GValue where
  parseJSON J.Null = return GValueNull
  parseJSON (J.String x) = return $ GValueString x
  parseJSON (J.Number x) = return $ either GValueDouble GValueInt $ floatingOrInteger x
  parseJSON (J.Bool x) = return $ GValueBool x
  parseJSON x = J.typeMismatch "GValue" x

makeGValue :: GValue -> IO G.GValue
makeGValue GValueNull       = GValue.buildGValue G.gtypePointer GValue.set_pointer nullPtr
makeGValue (GValueString x) = GValue.buildGValue G.gtypeString  GValue.set_string (Just x)
makeGValue (GValueInt    x) = GValue.buildGValue G.gtypeInt64   GValue.set_int64 (fromIntegral x)
makeGValue (GValueDouble x) = GValue.buildGValue G.gtypeDouble  GValue.set_double x
makeGValue (GValueBool   x) = GValue.buildGValue G.gtypeBoolean GValue.set_boolean x

type GObjectConfig = HM.HashMap T.Text GValue

data Config = Config
  { configSettings :: !GObjectConfig

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
  }

instance Default Config where
  def = either error id $ J.parseEither J.parseJSON J.emptyObject

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

some :: Maybe (ListOr a) -> [a]
some = maybe [] orList

instance J.FromJSON WK.CookieAcceptPolicy where
  parseJSON (J.Number 0) = return WK.CookieAcceptPolicyNever
  parseJSON (J.Number 1) = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.Number 2) = return WK.CookieAcceptPolicyAlways
  parseJSON (J.Bool False) = return WK.CookieAcceptPolicyNever
  parseJSON (J.Bool True) = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.String "never") = return WK.CookieAcceptPolicyNever
  parseJSON (J.String "Never") = return WK.CookieAcceptPolicyNever
  parseJSON (J.String "no-third-party") = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.String "NoThirdParty") = return WK.CookieAcceptPolicyNoThirdParty
  parseJSON (J.String "always") = return WK.CookieAcceptPolicyAlways
  parseJSON (J.String "Always") = return WK.CookieAcceptPolicyAlways
  parseJSON x = J.typeMismatch "CookieAcceptPolicy (never,no-third-party,always)" x

instance J.FromJSON WK.CacheModel where
  parseJSON (J.Number 0) = return WK.CacheModelDocumentViewer
  parseJSON (J.Bool False) = return WK.CacheModelDocumentViewer
  parseJSON (J.String "document-viewer") = return WK.CacheModelDocumentViewer
  parseJSON (J.String "DocumentViewer") = return WK.CacheModelDocumentViewer
  parseJSON (J.String "none") = return WK.CacheModelDocumentViewer
  parseJSON (J.Bool True) = return WK.CacheModelWebBrowser
  parseJSON (J.Number 2) = return WK.CacheModelWebBrowser
  parseJSON (J.String "web-browser") = return WK.CacheModelWebBrowser
  parseJSON (J.String "WebBrowser") = return WK.CacheModelWebBrowser
  parseJSON (J.String "web") = return WK.CacheModelWebBrowser
  parseJSON (J.String "all") = return WK.CacheModelWebBrowser
  parseJSON (J.Number 1) = return WK.CacheModelDocumentBrowser
  parseJSON (J.String "document-browser") = return WK.CacheModelDocumentBrowser
  parseJSON (J.String "DocumentBrowser") = return WK.CacheModelDocumentBrowser
  parseJSON (J.String "some") = return WK.CacheModelDocumentBrowser
  parseJSON x = J.typeMismatch "CacheModel (none,some,all)" x

instance J.FromJSON WK.ProcessModel where
  parseJSON (J.Bool False) = return WK.ProcessModelSharedSecondaryProcess
  parseJSON (J.Bool True) = return WK.ProcessModelMultipleSecondaryProcesses
  parseJSON (J.String "shared") = return WK.ProcessModelSharedSecondaryProcess
  parseJSON (J.String "multiple") = return WK.ProcessModelMultipleSecondaryProcesses
  parseJSON x = J.typeMismatch "ProcessModel (shared,multiple)" x

instance J.FromJSON Config where
  parseJSON = J.withObject "config" $ \c -> do
    settings            <-          c J..:! "settings" J..!= HM.empty
    styleSheet          <- some <$> c J..:! "style-sheet"
    script              <- some <$> c J..:! "script"
    dataDirectory       <-          c J..:! "data-directory"
    cookieFile          <-          c J..:! "cookie-file"
    cookieAcceptPolicy  <-          c J..:? "cookie-accept-policy" J..!= WK.CookieAcceptPolicyNever
    cacheModel          <-          c J..:! "cache-model" J..!= WK.CacheModelWebBrowser
    processCountLimit   <-          c J..:? "process-count-limit" J..!= 0
    proxy               <-          c J..:? "proxy"
    proxyIgnore         <- some <$> c J..:? "proxy-ignore"
    spellChecking       <-          c J..:! "spell-checking" J..!= True
    processModel        <-          c J..:! "process-model" J..!= WK.ProcessModelSharedSecondaryProcess
    charset             <-          c J..:? "charset"
    editable            <-          c J..:? "editable" J..!= False
    zoomLevel           <-          c J..:? "zoom-level" J..!= 1
    uri                 <-          c J..:? "uri"
    return Config
      { configSettings            = settings
      , configStyleSheet          = styleSheet
      , configScript              = script
      , configDataDirectory       = dataDirectory
      , configCookieFile          = cookieFile
      , configCookieAcceptPolicy  = cookieAcceptPolicy
      , configCacheModel          = cacheModel
      , configProcessCountLimit   = processCountLimit
      , configProxy               = proxy
      , configProxyIgnore         = proxyIgnore
      , configSpellChecking       = spellChecking
      , configProcessModel        = processModel
      , configCharset             = charset
      , configEditable            = editable
      , configZoomLevel           = zoomLevel
      , configURI                 = uri
      }
