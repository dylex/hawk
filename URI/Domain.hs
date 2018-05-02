{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI.Domain
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
import qualified Data.Aeson.Encoding as J (pair)
import qualified Data.Aeson.Internal as J ((<?>), JSONPathElement(Key))
import qualified Data.Aeson.Types as J (toJSONKeyText, typeMismatch)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Typed.Types as PG

import qualified URI.PrefixMap as PM
import qualified URI.ListMap as LM
import JS

type DomainComponent = T.Text
type DomainComponents = [DomainComponent]
newtype Domain = Domain{ domainComponents :: DomainComponents }
  deriving (Show)

splitDomain :: DomainComponent -> Domain
splitDomain = Domain . de . reverse . T.split ('.'==) where
  de (e:l) | T.null e = l
  de l = l

joinDomain :: Domain -> DomainComponent
joinDomain = T.intercalate (T.singleton '.') . reverse . domainComponents

splitStringDomain :: String -> Domain
splitStringDomain = Domain . spl where
  spl "" = []
  spl s = case q of { "" -> [] ; ~('.':r) -> spl r } ++ [T.pack p]
    where (p, q) = break ('.'==) s

instance IsString Domain where
  fromString = splitStringDomain

instance J.ToJSON Domain where
  toJSON = J.String . joinDomain
instance J.ToJSONKey Domain where
  toJSONKey = J.toJSONKeyText joinDomain

instance J.FromJSON Domain where
  parseJSON = J.withText "domain" (return . splitDomain)
instance J.FromJSONKey Domain where
  fromJSONKey = J.FromJSONKeyText splitDomain

instance PG.PGType "domainname" where
  type PGVal "domainname" = Domain
instance PG.PGParameter "domainname" Domain where
  pgEncode _ = TE.encodeUtf8 . joinDomain
instance PG.PGColumn "domainname" Domain where
  pgDecode _ = splitDomain . TE.decodeUtf8

domainRegExp :: T.Text -> T.Text
domainRegExp h = "^https?://(?:[^/?#]*\\.)?(" <> h <> ")\\.?(?::\\d+)?(?:[/?#]|$)"

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
  | otherwise = JSRegExp (domainRegExp $ T.intercalate (T.singleton '|') $ PM.foldTree df $ [] <$ m) True where
  df d l = [fn l <> quoteRegExp d]
  fn [] = T.empty
  fn l = altRegExp l <> "\\."

type DomainMap = LM.ListMap DomainComponent
type DomainSet = DomainMap ()

instance J.ToJSON1 DomainMap where
  liftToJSON to _ = J.object . map (\(k,v) -> joinDomain (Domain k) J..= to v) . LM.toList
  liftToEncoding to _ = J.pairs . foldMap (\(k,v) -> joinDomain (Domain k) `J.pair` to v) . LM.toList

instance J.FromJSON1 DomainMap where
  liftParseJSON parse _ (J.Object o) =
    LM.fromList <$> mapM (\(k,v) -> (domainComponents (splitDomain k) ,) <$> parse v J.<?> J.Key k) (HM.toList o)
  liftParseJSON parse _ v = LM.singleton [] <$> parse v

instance (J.ToJSON a) => J.ToJSON (DomainMap a) where
  toJSON = J.toJSON1
  toEncoding = J.toEncoding1

instance (J.FromJSON a) => J.FromJSON (DomainMap a) where
  parseJSON = J.parseJSON1
