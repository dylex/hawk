module GValue
  ( GValue(..)
  , makeGValue
  , setObjectProperty
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as J (null_)
import qualified Data.Aeson.Types as J (typeMismatch)
import qualified Data.GI.Base as G
import qualified Data.GI.Base.GValue as GValue
import qualified Data.GI.Base.Properties as GProp
import           Data.Int (Int64)
import           Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import           Foreign.Ptr (nullPtr)

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
