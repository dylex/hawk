module Script
  ( runScript
  , linkSelect
  , scriptMessageHandler
  , loadScripts
  ) where

import qualified Data.Aeson as J
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.WebKit2 as WK

import Types
import UI
import JS

scriptModule :: IsString s => s
scriptModule = "_HaWK__"

runScript :: T.Text -> HawkM ()
runScript s = do
  wv <- askWebView
  #runJavascript wv s (Nothing :: Maybe Gio.Cancellable) Nothing

builderText :: TLB.Builder -> T.Text
builderText = TL.toStrict . TLB.toLazyText

runScriptBuilder :: TLB.Builder -> HawkM ()
runScriptBuilder = runScript . builderText

callScript :: T.Text -> [JSValue] -> HawkM ()
callScript fun args = runScriptBuilder $
  scriptModule <> TLB.singleton '.' <> TLB.fromText fun
  <> TLB.singleton '(' <> buildJSValues args <> TLB.singleton ')'

linkSelect :: T.Text -> T.Text -> HawkM ()
linkSelect t r = callScript "linkSelect" [JSON (J.String t), JSRegExp r True]

scriptMessageHandler :: WK.JavascriptResult -> HawkM ()
scriptMessageHandler _arg =
  passThruBind Gdk.KEY_Escape

loadScripts :: HawkM ()
loadScripts = do
  cm <- askUserContentManager
  #removeAllScripts cm
  #addScript cm =<< asksGlobal globalScript
  mapM_ (#addScript cm) =<< asksGlobal hawkScript
