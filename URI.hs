{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI
  ( escapeURI
  ) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Typed.Types as PG
import           Network.URI (escapeURIChar)

type URI = T.Text

escapeURIChar' :: (Char -> Bool) -> Char -> String
escapeURIChar' p ' ' | not (p ' ') = "+"
escapeURIChar' p c = escapeURIChar p c

escapeURI :: (Char -> Bool) -> T.Text -> T.Text
escapeURI p = T.concatMap (T.pack . escapeURIChar' p)

instance PG.PGType "uri" where
  type PGVal "uri" = URI
instance PG.PGStringType "uri"
