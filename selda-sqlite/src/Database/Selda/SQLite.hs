{-# LANGUAGE GADTs, CPP, OverloadedStrings #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite
  ( SQLite
  , withSQLite
  , sqliteOpen, seldaClose
  , sqliteBackend
  , module Database.Selda
  ) where
import Database.Selda
import Database.Selda.Backend
import Data.Maybe (fromJust)
import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic
import Data.Text as Text (pack, toLower, take)
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.UUID.Types (toByteString)
import Database.SQLite3
import System.Directory (makeAbsolute)

data SQLite

-- | Open a new connection to an SQLite database.
--   The connection is reusable across calls to `runSeldaT`, and must be
--   explicitly closed using 'seldaClose' when no longer needed.
sqliteOpen :: (MonadIO m, MonadMask m) => FilePath -> m (SeldaConnection SQLite)
sqliteOpen file = do
  mask $ \restore -> do
    edb <- try $ liftIO $ open (pack file)
    case edb of
      Left e@(SQLError{}) -> do
        throwM (DbError (show e))
      Right db -> flip onException (liftIO (close db)) . restore $ do
        absFile <- liftIO $ pack <$> makeAbsolute file
        let backend = mkBackend db
        void . liftIO $ runStmt backend "PRAGMA foreign_keys = ON;" []
        newConnection backend absFile

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT SQLite m a -> m a
withSQLite file m = bracket (sqliteOpen file) seldaClose (runSeldaT m)

sqliteBackend :: Database -> SeldaBackend SQLite
sqliteBackend = mkBackend
