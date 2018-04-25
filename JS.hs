module JS
  ( JSValue(..)
  , buildJSValues
  , buildJSValue
  , quoteRegExp
  , altRegExp
  , setObjPropertiesBuilder
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Text as JT
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V

import Util

data JSValue
  = JSON !J.Value
  | JSObject !(HM.HashMap T.Text JSValue)
  | JSArray !(V.Vector JSValue)
  | JSRegExp
    { jsRegExpSource :: !T.Text
    , jsRegExpIgnoreCase :: !Bool
      -- TODO: other flags
    }

instance J.FromJSON JSValue where
  parseJSON = return . JSON

commas :: (a -> TLB.Builder) -> [a] -> TLB.Builder
commas f = mintersperse (TLB.singleton ',') . map f

buildJSValue :: JSValue -> TLB.Builder
buildJSValue (JSON j) = JT.encodeToTextBuilder j
buildJSValue (JSObject o) = TLB.singleton '{' <> commas f (HM.toList o) <> TLB.singleton '}' where
  f (k, v) = JT.encodeToTextBuilder (J.String k) <> TLB.singleton ':' <> buildJSValue v
buildJSValue (JSArray a) = TLB.singleton '[' <> buildJSValues (V.toList a) <> TLB.singleton ']'
buildJSValue r@JSRegExp{ jsRegExpSource = s } =
  TLB.singleton '/' <> TLB.fromText (T.replace "/" "\\/" s) <> TLB.singleton '/' <> flags
  where
  flags = if jsRegExpIgnoreCase r then TLB.singleton 'i' else mempty

buildJSValues :: [JSValue] -> TLB.Builder
buildJSValues = commas buildJSValue

quoteRegExp :: T.Text -> T.Text
quoteRegExp = T.concatMap esc where
  esc c
    | c `elem` ("$()*+.?[\\]^{|}" :: String) = T.pack ['\\', c]
    | otherwise = T.singleton c

altRegExp :: [T.Text] -> T.Text
altRegExp [] = T.empty
altRegExp [x] = x
altRegExp l = "(?:" <> T.intercalate (T.singleton '|') l <> ")"

setObjPropertiesBuilder :: T.Text -> HM.HashMap T.Text JSValue -> TLB.Builder
setObjPropertiesBuilder obj = HM.foldrWithKey (\k v b ->
  TLB.fromText obj <> TLB.singleton '[' <> JT.encodeToTextBuilder (J.String k) <> "]=" <> buildJSValue v <> TLB.singleton ';' <> b) mempty

