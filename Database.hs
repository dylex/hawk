{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database
  ( Database
  , defaultDatabase
  , databaseOpen
  , databaseClose
  ) where

import qualified Database.PostgreSQL.Typed as PG

import Config

type Database = PG.PGDatabase

PG.useTPGDatabase defaultDatabase

databaseOpen :: PG.PGDatabase -> IO PG.PGConnection
databaseOpen = PG.pgConnect

databaseClose :: PG.PGConnection -> IO ()
databaseClose = PG.pgDisconnect
