module Script
  ( JSValue(..)
  , runScript
  , linkSelect
  ) where

import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Text as JT
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import qualified GI.Gio as Gio

import JSON (mintersperse)
import Types

scriptModule :: IsString s => s
scriptModule = "__HaWK__"

runScript :: T.Text -> HawkM ()
runScript s = do
  wv <- asks hawkWebView
  #runJavascript wv s Gio.noCancellable Nothing

data JSValue
  = JSON !J.Value
  | JSRegExp
    { jsRegExpSource :: !T.Text
    , jsRegExpIgnoreCase :: !Bool
    -- TODO: flags
    }

instance J.FromJSON JSValue where
  parseJSON = return . JSON

buildJSValue :: JSValue -> TLB.Builder
buildJSValue (JSON j) = JT.encodeToTextBuilder j
buildJSValue r@JSRegExp{ jsRegExpSource = s } =
  TLB.singleton '/' <> TLB.fromText (T.replace "/" "\\/" s) <> TLB.singleton '/' <> flags
  where
  flags = if jsRegExpIgnoreCase r then TLB.singleton 'i' else mempty

runScriptBuilder :: TLB.Builder -> HawkM ()
runScriptBuilder = runScript . TL.toStrict . TLB.toLazyText

callScript :: T.Text -> [JSValue] -> HawkM ()
callScript fun args = runScriptBuilder $
  scriptModule <> TLB.singleton '.' <> TLB.fromText fun
  <> TLB.singleton '(' <> mintersperse (TLB.singleton ',') (map buildJSValue args) <> TLB.singleton ')'

linkSelect :: T.Text -> T.Text -> HawkM ()
linkSelect t r = callScript "linkSelect" [JSON (J.String t), JSRegExp r True]

