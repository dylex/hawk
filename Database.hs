{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database
  ( Database
  , databaseOpen
  , databaseClose
  ) where

import           Data.Default (def)
import qualified Database.PostgreSQL.Typed as PG

import Config ()

type Database = PG.PGDatabase

PG.useTPGDatabase def

databaseOpen :: PG.PGDatabase -> IO PG.PGConnection
databaseOpen = PG.pgConnect

databaseClose :: PG.PGConnection -> IO ()
databaseClose = PG.pgDisconnect
