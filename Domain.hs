{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Domain
  ( Domain(..)
  , splitDomain
  , splitStringDomain
  , joinDomain
  , DomainPMap
  , DomainPSet
  , domainPSetRegExp
  , DomainMap
  , DomainSet
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Internal as J ((<?>), JSONPathElement(Key))
import qualified Data.Aeson.Types as J (typeMismatch, toJSONKeyText, contramapToJSONKeyFunction)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Typed.Types as PG

import qualified Data.PrefixMap as PM
import qualified Data.ListMap as LM
import JS

newtype DomainComponent = DomainComponent{ domainComponentText :: T.Text }
  deriving (Eq, Ord, Hashable, Show, IsString)
type DomainComponents = [DomainComponent]
newtype Domain = Domain{ domainComponents :: DomainComponents }
  deriving (Show)

splitDomain :: T.Text -> Domain
splitDomain = Domain . map DomainComponent . de . reverse . T.split ('.'==) where
  de (e:l) | T.null e = l
  de l = l

joinDomain :: Domain -> T.Text
joinDomain = T.intercalate (T.singleton '.') . reverse . map domainComponentText . domainComponents

splitStringDomain :: String -> Domain
splitStringDomain = Domain . spl where
  spl "" = []
  spl s = case q of { "" -> [] ; ~('.':r) -> spl r } ++ [DomainComponent $ T.pack p]
    where (p, q) = break ('.'==) s

instance IsString Domain where
  fromString = splitStringDomain

instance J.ToJSON Domain where
  toJSON = J.String . joinDomain
instance J.ToJSONKey Domain where
  toJSONKey = J.toJSONKeyText joinDomain

instance J.ToJSON DomainComponent where
  toJSON     = J.String . domainComponentText
  toEncoding = JE.text  . domainComponentText
  toJSONList     = J.toJSON     . Domain
  toEncodingList = J.toEncoding . Domain
instance J.ToJSONKey DomainComponent where
  toJSONKey = J.toJSONKeyText domainComponentText
  toJSONKeyList = J.contramapToJSONKeyFunction Domain J.toJSONKey

instance J.FromJSON Domain where
  parseJSON = J.withText "domain" (return . splitDomain)
instance J.FromJSONKey Domain where
  fromJSONKey = J.FromJSONKeyText splitDomain

instance J.FromJSON DomainComponent where
  parseJSON = J.withText "domain component" (return . DomainComponent)
  parseJSONList = fmap domainComponents . J.parseJSON
instance J.FromJSONKey DomainComponent where
  fromJSONKey = J.FromJSONKeyText DomainComponent
  fromJSONKeyList = domainComponents <$> J.fromJSONKey

instance PG.PGType "domainname" where
  type PGVal "domainname" = Domain
instance PG.PGParameter "domainname" Domain where
  pgEncode _ = TE.encodeUtf8 . joinDomain
instance PG.PGColumn "domainname" Domain where
  pgDecode _ = splitDomain . TE.decodeUtf8

type DomainPMap = PM.PrefixMap DomainComponent
type DomainPSet = DomainPMap ()

instance J.ToJSON (DomainPMap ()) where
  toJSON = J.toJSON . map (J.toJSON . Domain . fst) . PM.toList

instance J.FromJSON (DomainPMap ()) where
  parseJSON J.Null = return PM.empty
  parseJSON (J.Bool True) = return $ PM.singleton [] ()
  parseJSON (J.Bool False) = return PM.empty
  parseJSON (J.Array l) = PM.fromList <$> V.mapM (fmap ((, ()) . domainComponents) . J.parseJSON) l
  parseJSON v = J.typeMismatch "domain set" v

domainPSetRegExp :: DomainPMap a -> JSValue
domainPSetRegExp m
  | PM.null m = JSON J.Null
  | otherwise = JSRegExp ("^https?://(?:[^/?#]*\\.)?("
    <> T.intercalate (T.singleton '|') (PM.foldTree df $ [] <$ m)
    <> ")\\.?(?::\\d+)?(?:[/?#]|$)") True where
  df (DomainComponent d) l = [fn l <> quoteRegExp d]
  fn [] = T.empty
  fn l = altRegExp l <> "\\."

type DomainMap = LM.ListMap DomainComponent
type DomainSet = DomainMap ()

{-
instance J.ToJSON1 DomainMap where
  liftToJSON to _ = J.object . map (\(k,v) -> joinDomain (Domain k) J..= to v) . LM.toList
  liftToEncoding to _ = J.pairs . foldMap (\(k,v) -> joinDomain (Domain k) `JE.pair` to v) . LM.toList

instance J.FromJSON1 DomainMap where
  liftParseJSON parse _ (J.Object o) =
    LM.fromList <$> mapM (\(k,v) -> (domainComponents (splitDomain k) ,) <$> parse v J.<?> J.Key k) (HM.toList o)
  liftParseJSON parse _ v = LM.singleton [] <$> parse v

instance (J.ToJSON a) => J.ToJSON (DomainMap a) where
  toJSON = J.toJSON1
  toEncoding = J.toEncoding1

instance (J.FromJSON a) => J.FromJSON (DomainMap a) where
  parseJSON = J.parseJSON1
  -}
