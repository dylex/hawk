{-# LANGUAGE FlexibleContexts #-}

module Database
  ( hawkDBQuery
  , hawkDBExecute
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Database.PostgreSQL.Typed.Query (PGQuery, pgQuery, pgExecute)

import Util
import Types

hawkDBQuery :: PGQuery q a => q -> HawkM [a]
hawkDBQuery q = liftIO . foldMapA (`pgQuery` q) =<< asksGlobal hawkDatabase

hawkDBExecute :: PGQuery q () => q -> HawkM ()
hawkDBExecute q = liftIO . mapM_ (`pgExecute` q) =<< asksGlobal hawkDatabase
