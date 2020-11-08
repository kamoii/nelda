{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.SQLite where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket, mask, onException, throwM, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (pack)
import Database.Nelda.Backend.Connection (NeldaConnection, closeNeldaConnection, newNeldaConnection)
import Database.Nelda.Backend.Monad (NeldaT, runNeldaT)
import Database.Nelda.Backend.Runner (runStmt)
import Database.Nelda.Backend.Types (BackendError (DbError))
import Database.SQLite3 (SQLError (SQLError), close, open)
import System.Directory (makeAbsolute)

-- | Open a new connection to an SQLite database.
--   The connection is reusable across calls to `runNeldaT`, and must be
--   explicitly closed using 'neldaClose' when no longer needed.
sqliteOpen :: (MonadIO m, MonadMask m) => FilePath -> m NeldaConnection
sqliteOpen file =
    mask $ \restore -> do
        edb <- try $ liftIO $ open (pack file)
        case edb of
            Left e@(SQLError{}) -> do
                throwM (DbError (show e))
            Right conn -> flip onException (liftIO (close conn)) . restore $ do
                absFile <- liftIO $ pack <$> makeAbsolute file
                void . liftIO $ runStmt conn "PRAGMA foreign_keys = ON;" []
                newNeldaConnection conn absFile

sqliteClose :: MonadIO m => NeldaConnection -> m ()
sqliteClose = closeNeldaConnection

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> NeldaT m a -> m a
withSQLite file m = bracket (sqliteOpen file) sqliteClose (runNeldaT m)
