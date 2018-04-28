module Database
  ( hawkQuery
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Database.PostgreSQL.Typed.Query (PGQuery, pgQuery)

import Util
import Types

hawkQuery :: PGQuery q a => q -> HawkM [a]
hawkQuery q = liftIO . foldMapA (`pgQuery` q) =<< asks hawkDatabase
