{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, CPP, TypeFamilies #-}
-- | Internal backend API.
--   Using anything exported from this module may or may not invalidate any
--   safety guarantees made by Selda; use at your own peril.
module Database.Selda.Backend.Internal
  ( StmtID (..), BackendID (..)
  , QueryRunner, SeldaBackend (..), SeldaConnection (..), SeldaStmt (..)
  , MonadSelda (..), SeldaT (..), SeldaM
  , SeldaError (..)
  , Param (..), Lit (..), ColAttr (..), AutoIncType (..)
  , SqlType (..), SqlValue (..), SqlTypeRep (..)
  , TableInfo (..), ColumnInfo (..), tableInfo, fromColInfo
  , isAutoPrimary, isPrimary, isUnique
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  , freshStmtId
  , newConnection, allStmts
  , runSeldaT, withBackend
  ) where
import Data.List (nub)
import Data.Coerce (coerce)
import Database.Selda.Core.Types
import qualified Database.Selda.Backend.Connection as BC
import Database.Selda.Backend.Types (SqlParam, SqlValue)
import Database.Selda.Backend.Connection (PreparedStatement)
import Database.Selda.SQL (Param (..), paramToSqlParam)
import Database.Selda.SqlType
import Database.Selda.Table hiding (colName, colType, colFKs)
import qualified Database.Selda.Table as Table (ColInfo (..))
import Database.Selda.SQL.Print.Config
import Database.Selda.Types (TableName, ColName)
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.IntMap as M
import Data.IORef
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

{-# NOINLINE nextStmtId #-}
nextStmtId :: IORef Int
nextStmtId = unsafePerformIO $ newIORef 1

-- | Generate a fresh statement identifier, guaranteed to be unique per process.
freshStmtId :: MonadIO m => m StmtID
freshStmtId = liftIO $ atomicModifyIORef' nextStmtId $ \n -> (n+1, StmtID n)

-- | A function which executes a query and gives back a list of extensible
--   tuples; one tuple per result row, and one tuple element per column.
type QueryRunner a = Text -> [Param] -> IO a

-- | A prepared statement.
data SeldaStmt = SeldaStmt
 { -- | Backend-specific handle to the prepared statement.
   stmtHandle :: !PreparedStatement

   -- | The SQL code for the statement.
 , stmtText :: !Text

   -- | All parameters to be passed to the prepared statement.
   --   Parameters that are unique to each invocation are specified as indices
   --   starting at 0.
   --   Backends implementing @runPrepared@ should probably ignore this field.
 , stmtParams :: ![Either Int Param]
 }

data SeldaConnection b = SeldaConnection
  { -- | The backend used by the current connection.
    connBackend :: !(SeldaBackend b)

    -- | A string uniquely identifying the database used by this connection.
    --   This could be, for instance, a PostgreSQL connection
    --   string or the absolute path to an SQLite file.
  , connDbId :: Text

    -- | All statements prepared for this connection.
  , connStmts :: !(IORef (M.IntMap SeldaStmt))

    -- | Is the connection closed?
  , connClosed :: !(IORef Bool)

    -- | Lock to prevent this connection from being used concurrently by
    --   multiple invocations of 'runSeldaT'.
  , connLock :: !(MVar ())
}

-- | Create a new Selda connection for the given backend and database
--   identifier string.
newConnection :: MonadIO m => SeldaBackend b -> Text -> m (SeldaConnection b)
newConnection back dbid =
  liftIO $ SeldaConnection back dbid <$> newIORef M.empty
                                     <*> newIORef False
                                     <*> newMVar ()

-- | Get all statements and their corresponding identifiers for the current
--   connection.
allStmts :: SeldaConnection b -> IO [(StmtID, PreparedStatement)]
allStmts = fmap (map (\(k, v) -> (StmtID k, stmtHandle v)) . M.toList)
  . readIORef
  . connStmts

-- | Comprehensive information about a table.
type TableInfo = TableInfo' SqlTypeRep

-- | Comprehensive information about a column.
type ColumnInfo = ColumnInfo' SqlTypeRep

-- | Convert a 'Table.ColInfo' into a 'ColumnInfo'.
fromColInfo :: Table.ColInfo -> ColumnInfo
fromColInfo ci = ColumnInfo
    { colName = Table.colName ci
    , colType = Right $ Table.colType ci
    , colIsAutoPrimary = any isAutoPrimary (Table.colAttrs ci)
    , colIsNullable = Optional `elem` Table.colAttrs ci
    , colHasIndex = not $ null [() | Indexed _ <- Table.colAttrs ci]
    , colFKs = map fk (Table.colFKs ci)
    }
  where
    fk (Table tbl _ _ _, col) = (tbl, col)

-- | Get the column information for each column in the given table.
tableInfo :: Table a -> TableInfo
tableInfo t = TableInfo
  { tableColumnInfos = map fromColInfo (tableCols t)
  , tableUniqueGroups = uniqueGroups
  , tablePrimaryKey = pkGroup
  }
  where
    uniqueGroups =
      [ map (Table.colName . ((tableCols t) !!)) ixs
      | (ixs, Unique) <- tableAttrs t
      ]
    pkGroup = nub $ concat
      [ concat
        [ map (Table.colName . ((tableCols t) !!)) ixs
        | (ixs, attr) <- tableAttrs t
        , isPrimary attr
        ]
      , [ Table.colName col
        | col <- tableCols t
        , attr <- Table.colAttrs col
        , isPrimary attr
        ]
      ]

-- | A collection of functions making up a Selda backend.
data SeldaBackend b = SeldaBackend
  { -- | Execute an SQL statement.
    runStmt :: Text -> [SqlParam] -> IO (Int, [[SqlValue]])

    -- | Execute an SQL statement and return the last inserted primary key,
    --   where the primary key is auto-incrementing.
    --   Backends must take special care to make this thread-safe.
  , runStmtWithPK :: Text -> [SqlParam] -> IO Int

    -- | Prepare a statement using the given statement identifier.
  , prepareStmt :: StmtID -> [SqlTypeRep] -> Text -> IO PreparedStatement

    -- | Execute a prepared statement.
  , runPrepared :: PreparedStatement -> [SqlParam] -> IO (Int, [[SqlValue]])

    -- | Get a list of all columns in the given table, with the type and any
    --   modifiers for each column.
    --   Return an empty list if the given table does not exist.
  , getTableInfo :: TableName -> IO TableInfo

    -- | Close the currently open connection.
  , closeConnection :: IO ()

    -- | Unique identifier for this backend.
  , backendId :: BackendID

    -- | Turn on or off foreign key checking, and initiate/commit
    --   a transaction.
    --
    --   When implementing this function, it is safe to assume that
    --   @disableForeignKeys True@
    --   will always be called exactly once before each
    --   @disableForeignKeys False@.
  , disableForeignKeys :: Bool -> IO ()
  }

-- b は forall だが,実装上 initiate する backend によって一意的に決まるはず。
mkBackend :: BC.Connection -> SeldaBackend b
mkBackend conn = SeldaBackend
    { runStmt = BC.runStmt conn
    , runStmtWithPK = BC.runStmtWithPK conn
    , prepareStmt = BC.prepareStmt conn
    , runPrepared = BC.runPrepared conn
    , getTableInfo = BC.getTableInfo conn
    , closeConnection = BC.closeConnection conn
    , backendId = BC.backendId
    , disableForeignKeys = BC.disableForeignKeys conn
    }

-- | Some monad with Selda SQL capabilitites.
class MonadIO m => MonadSelda m where
  {-# MINIMAL withConnection #-}

  -- | Type of database backend used by @m@.
  type Backend m

  -- | Pass a Selda connection to the given computation and execute it.
  --   After the computation finishes, @withConnection@ is free to do anything
  --   it likes to the connection, including closing it or giving it to another
  --   Selda computation.
  --   Thus, the computation must take care never to return or otherwise
  --   access the connection after returning.
  withConnection :: (SeldaConnection (Backend m) -> m a) -> m a

  -- | Perform the given computation as a transaction.
  --   Implementations must ensure that subsequent calls to 'withConnection'
  --   within the same transaction always passes the same connection
  --   to its argument.
  transact :: m a -> m a
  transact = id

-- | Get the backend in use by the computation.
withBackend :: MonadSelda m => (SeldaBackend (Backend m) -> m a) -> m a
withBackend m = withConnection (m . connBackend)

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT b m a = S {unS :: StateT (SeldaConnection b) m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask , MonadFail
           )

instance (MonadIO m, MonadMask m) => MonadSelda (SeldaT b m) where
  type Backend (SeldaT b m) = b
  withConnection m = S get >>= m

instance MonadTrans (SeldaT b) where
  lift = S . lift

-- | The simplest form of Selda computation; 'SeldaT' specialized to 'IO'.
type SeldaM b = SeldaT b IO

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: (MonadIO m, MonadMask m)
          => SeldaT b m a
          -> SeldaConnection b
          -> m a
runSeldaT m c =
    bracket (liftIO $ takeMVar (connLock c))
            (const $ liftIO $ putMVar (connLock c) ())
            (const go)
  where
    go = do
      closed <- liftIO $ readIORef (connClosed c)
      when closed $ do
        liftIO $ throwM $ DbError "runSeldaT called with a closed connection"
      fst <$> runStateT (unS m) c
