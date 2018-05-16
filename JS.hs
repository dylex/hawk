module JS
  ( JSValue(..)
  , buildJSValues
  , buildJSValue
  , quoteRegExp
  , altRegExp
  , setObjPropertiesBuilder
  , LoadElement(..)
  , loadElementName
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Text as JT
import           Data.Char (isDigit, isAlphaNum)
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
commas = mintersperseMap (TLB.singleton ',')

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

isIdentifier :: T.Text -> Bool
isIdentifier s = not (T.null s) && not (isDigit (T.head s)) && T.all isJSIdent s where
  isJSIdent '_' = True
  isJSIdent '$' = True
  isJSIdent x = isAlphaNum x

propertyRef :: T.Text -> TLB.Builder
propertyRef p
  | isIdentifier p = TLB.singleton '.' <> TLB.fromText p
  | otherwise = TLB.singleton '[' <> JT.encodeToTextBuilder (J.String p) <> TLB.singleton ']'

setObjPropertiesBuilder :: T.Text -> [(T.Text, JSValue)] -> TLB.Builder
setObjPropertiesBuilder obj = foldMap (\(k, v) ->
  TLB.fromText obj <> propertyRef k <> TLB.singleton '=' <> buildJSValue v <> TLB.singleton ';')

data LoadElement
  = LoadFRAME
  | LoadIFRAME
  | LoadIMG
  | LoadINPUT
  | LoadLINK
  | LoadSCRIPT
  deriving (Eq, Enum, Bounded)

loadElementName :: LoadElement -> T.Text
loadElementName LoadFRAME  = "FRAME"
loadElementName LoadIFRAME = "IFRAME"
loadElementName LoadIMG    = "IMG"
loadElementName LoadINPUT  = "INPUT"
loadElementName LoadLINK   = "LINK"
loadElementName LoadSCRIPT = "SCRIPT"

instance J.ToJSON LoadElement where
  toJSON = J.String . loadElementName

instance J.FromJSON LoadElement where
  parseJSON = J.withText "load element" $ ple . T.toUpper where
    ple "FRAME"  = return LoadFRAME
    ple "IFRAME" = return LoadIFRAME
    ple "IMG"    = return LoadIMG
    ple "INPUT"  = return LoadINPUT
    ple "LINK"   = return LoadLINK
    ple "SCRIPT" = return LoadSCRIPT
    ple _ = fail "Unknown load element"
