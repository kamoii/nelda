{-# LANGUAGE OverloadedStrings, RecordWildCards, GADTs, CPP #-}
-- | PostgreSQL backend for Selda.
module Database.Selda.PostgreSQL
  ( PG, PGConnectInfo (..)
  , withPostgreSQL, on, auth
  , pgOpen, pgOpen', seldaClose
  , pgConnString
  , module Database.Selda
  ) where
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid
#endif
import Data.ByteString (ByteString)
import Data.String (IsString (..))
import qualified Data.Text as T
import Database.Selda
import Database.Selda.Backend hiding (toText)
-- import Database.Selda.JSON
import Database.Selda.Unsafe as Selda (cast, operator)
import Control.Monad.Catch
import Control.Monad.IO.Class

import Control.Monad (void)
import qualified Data.ByteString as BS (foldl')
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Dynamic
import Data.Foldable (for_)
import Data.Text.Encoding
-- import Database.Selda.PostgreSQL.Encoding
import Database.PostgreSQL.LibPQ hiding (user, pass, db, host)

data PG

-- instance JSONBackend PG where
--   (~>) = operator "->"
--   jsonToText = Selda.cast

-- | PostgreSQL connection information.
data PGConnectInfo = PGConnectInfo
  { -- | Host to connect to.
    pgHost     :: T.Text
    -- | Port to connect to.
  , pgPort     :: Int
    -- | Name of database to use.
  , pgDatabase :: T.Text
    -- | Schema to use upon connection.
  , pgSchema   :: Maybe T.Text
    -- | Username for authentication, if necessary.
  , pgUsername :: Maybe T.Text
    -- | Password for authentication, if necessary.
  , pgPassword :: Maybe T.Text
  }
  | PGConnectionString
  { -- | Custom connection PostgreSQL connection string.
    pgConnectionString :: T.Text
  , pgSchema :: Maybe T.Text
  }

instance IsString PGConnectInfo where
  fromString s = PGConnectionString
    { pgConnectionString = fromString s
    , pgSchema = Nothing
    }

-- | Connect to the given database on the given host, on the default PostgreSQL
--   port (5432):
--
-- > withPostgreSQL ("my_db" `on` "example.com") $ do
-- >   ...
on :: T.Text -> T.Text -> PGConnectInfo
on db host = PGConnectInfo
  { pgHost = host
  , pgPort = 5432
  , pgDatabase = db
  , pgSchema   = Nothing
  , pgUsername = Nothing
  , pgPassword = Nothing
  }
infixl 7 `on`

-- | Add the given username and password to the given connection information:
--
-- > withPostgreSQL ("my_db" `on` "example.com" `auth` ("user", "pass")) $ do
-- >   ...
--
--   For more precise control over the connection options, you should modify
--   the 'PGConnectInfo' directly.
auth :: PGConnectInfo -> (T.Text, T.Text) -> PGConnectInfo
auth ci (user, pass) = ci
  { pgUsername = Just user
  , pgPassword = Just pass
  }
infixl 4 `auth`

-- | Convert `PGConnectInfo` into `ByteString`
pgConnString :: PGConnectInfo -> ByteString
pgConnString PGConnectInfo{..} = mconcat
  [ "host=", encodeUtf8 pgHost, " "
  , "port=", BS.pack (show pgPort), " "
  , "dbname=", encodeUtf8 pgDatabase, " "
  , case pgUsername of
      Just user -> "user=" <> encodeUtf8 user <> " "
      _         -> ""
  , case pgPassword of
      Just pass -> "password=" <> encodeUtf8 pass <> " "
      _         -> ""
  , "connect_timeout=10", " "
  , "client_encoding=UTF8"
  ]
pgConnString PGConnectionString{..} = encodeUtf8 pgConnectionString

-- | Perform the given computation over a PostgreSQL database.
--   The database connection is guaranteed to be closed when the computation
--   terminates.
withPostgreSQL :: (MonadIO m, MonadMask m)
               => PGConnectInfo
               -> SeldaT PG m a
               -> m a
withPostgreSQL ci m = bracket (pgOpen ci) seldaClose (runSeldaT m)

-- | Open a new PostgreSQL connection. The connection will persist across
--   calls to 'runSeldaT', and must be explicitly closed using 'seldaClose'
--   when no longer needed.
pgOpen :: (MonadIO m, MonadMask m) => PGConnectInfo -> m (SeldaConnection PG)
pgOpen ci = pgOpen' (pgSchema ci) (pgConnString ci)

pgOpen' :: (MonadIO m, MonadMask m)
        => Maybe T.Text
        -> ByteString
        -> m (SeldaConnection PG)
pgOpen' schema connStr =
  bracketOnError (liftIO $ connectdb connStr) (liftIO . finish) $ \conn -> do
    st <- liftIO $ status conn
    case st of
      ConnectionOk -> do
        let backend = mkBackend conn

        _ <- liftIO $ runStmt backend "SET client_min_messages TO WARNING;" []

        for_ schema $ \schema' ->
          liftIO $ runStmt backend ("SET search_path TO '" <> schema' <> "';") []

        newConnection backend (decodeUtf8 connStr)
      nope -> do
        connFailed nope
    where
      connFailed f = throwM $ DbError $ unwords
        [ "unable to connect to postgres server: " ++ show f
        ]
