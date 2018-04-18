module Script
  ( runScript
  , callScript
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

runScriptBuilder :: TLB.Builder -> HawkM ()
runScriptBuilder = runScript . TL.toStrict . TLB.toLazyText

callScript :: T.Text -> [J.Value] -> HawkM ()
callScript fun args = runScriptBuilder $
  scriptModule <> "." <> TLB.fromText fun <> "(" <> mintersperse "," (map JT.encodeToTextBuilder args) <> ")"
