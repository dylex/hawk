{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI.Domain
  ( Domain(..)
  , splitDomain
  , joinDomain
  , DomainMap
  , DomainSet
  , domainSetRegExp
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (listParser)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Typed.Types as PG

import qualified URI.PrefixMap as PM
import JS

type DomainComponent = T.Text
type DomainComponents = [DomainComponent]
newtype Domain = Domain{ domainComponents :: DomainComponents }

splitDomain :: DomainComponent -> Domain
splitDomain = Domain . de . reverse . T.split ('.'==) where
  de (e:l) | T.null e = l
  de l = l

joinDomain :: Domain -> DomainComponent
joinDomain = T.intercalate (T.singleton '.') . reverse . domainComponents

instance J.ToJSON Domain where
  toJSON = J.String . joinDomain

instance J.FromJSON Domain where
  parseJSON = J.withText "domain" (return . splitDomain)

instance PG.PGType "domainname" where
  type PGVal "domainname" = Domain
instance PG.PGParameter "domainname" Domain where
  pgEncode _ = TE.encodeUtf8 . joinDomain
instance PG.PGColumn "domainname" Domain where
  pgDecode _ = splitDomain . TE.decodeUtf8

domainRegExp :: T.Text -> T.Text
domainRegExp h = "^https?://(?:[^/?#]*\\.)?(" <> h <> ")\\.?(?::\\d+)?(?:[/?#]|$)"

type DomainMap a = PM.PrefixMap DomainComponent a
type DomainSet = DomainMap ()

instance J.ToJSON (DomainMap ()) where
  toJSON = J.toJSON . map (J.toJSON . Domain . fst) . PM.toList

instance J.FromJSON (DomainMap ()) where
  parseJSON = fmap PM.fromList . J.listParser (fmap ((, ()) . domainComponents) . J.parseJSON)

domainSetRegExp :: DomainMap a -> JSValue
domainSetRegExp m
  | PM.null m = JSON J.Null
  | otherwise = JSRegExp (domainRegExp $ T.intercalate (T.singleton '|') $ PM.foldTree df $ [] <$ m) True where
  df d l = [fn l <> quoteRegExp d]
  fn [] = T.empty
  fn l = altRegExp l <> "\\."
