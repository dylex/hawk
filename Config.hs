module Config
  ( defaultDatabase
  ) where

import           Network (PortID(..))
import           Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase)

defaultDatabase :: PGDatabase
defaultDatabase = defaultPGDatabase
  { pgDBUser = "dylan"
  , pgDBName = "dylan"
  , pgDBPort = UnixSocket "/tmp/.s.PGSQL.5432"
  }
