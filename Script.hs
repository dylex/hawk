module Script
  ( setPropertiesScript
  , runScript
  , linkSelect
  , scriptMessageHandler
  ) where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import qualified GI.Gio as Gio
import qualified GI.WebKit2 as WK

import Types
import UI
import JS

scriptModule :: IsString s => s
scriptModule = "_HaWK__"

runScript :: T.Text -> HawkM ()
runScript s = do
  wv <- askWebView
  #runJavascript wv s Gio.noCancellable Nothing

setPropertiesBuilder :: HM.HashMap T.Text JSValue -> TLB.Builder
setPropertiesBuilder = setObjPropertiesBuilder scriptModule

setPropertiesScript :: HM.HashMap T.Text JSValue -> T.Text
setPropertiesScript = TL.toStrict . TLB.toLazyText . setPropertiesBuilder

runScriptBuilder :: TLB.Builder -> HawkM ()
runScriptBuilder = runScript . TL.toStrict . TLB.toLazyText

callScript :: T.Text -> [JSValue] -> HawkM ()
callScript fun args = runScriptBuilder $
  scriptModule <> TLB.singleton '.' <> TLB.fromText fun
  <> TLB.singleton '(' <> buildJSValues args <> TLB.singleton ')'

linkSelect :: T.Text -> T.Text -> HawkM ()
linkSelect t r = callScript "linkSelect" [JSON (J.String t), JSRegExp r True]

scriptMessageHandler :: WK.JavascriptResult -> HawkM ()
scriptMessageHandler _arg =
  passThruBind
