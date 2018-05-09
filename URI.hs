{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI
  ( escapeURI
  , uriDomain
  ) where

import           Control.Monad ((<=<))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Typed.Types as PG
import qualified Network.URI as U

import Domain

type URI = T.Text

escapeURIChar' :: (Char -> Bool) -> Char -> String
escapeURIChar' p ' ' | not (p ' ') = "+"
escapeURIChar' p c = U.escapeURIChar p c

escapeURI :: (Char -> Bool) -> T.Text -> T.Text
escapeURI p = T.concatMap (T.pack . escapeURIChar' p)

instance PG.PGType "uri" where
  type PGVal "uri" = URI
instance {-# OVERLAPPING #-} PG.PGParameter "uri" URI where
  pgEncode _ = TE.encodeUtf8
instance {-# OVERLAPPING #-} PG.PGColumn "uri" URI where
  pgDecode _ = TE.decodeUtf8

uriDomain :: URI -> Maybe Domain
uriDomain = fmap (splitStringDomain . U.uriRegName) . (U.uriAuthority <=< U.parseURI) . T.unpack
