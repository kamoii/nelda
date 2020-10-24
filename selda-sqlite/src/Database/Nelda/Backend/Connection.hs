module Database.Nelda.Backend.Connection where

import Database.Nelda.Backend.Types (Statement, Connection)
import Database.Nelda.Backend.Statement (stmtHandle, StmtID(..), NeldaStmt)
import Data.Text (Text)
import Data.IORef (atomicModifyIORef', readIORef, newIORef, IORef)
import qualified Data.IntMap as M
import Control.Concurrent (newMVar, MVar)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch (mask_)
import Control.Monad (unless)
import Database.Nelda.Backend.Runner (closeConnection)

data NeldaConnection = NeldaConnection
    { -- | The backend used by the current connection.
      connConnection :: !Connection

      -- | A string uniquely identifying the database used by this connection.
      --   This could be, for instance, a PostgreSQL connection
      --   string or the absolute path to an SQLite file.
    , connDbId :: Text

    -- | All statements prepared for this connection.
    , connStmts :: !(IORef (M.IntMap NeldaStmt))

    -- | Is the connection closed?
    , connClosed :: !(IORef Bool)

    -- | Lock to prevent this connection from being used concurrently by
    --   multiple invocations of 'runSeldaT'.
    , connLock :: !(MVar ())
    }

-- | Create a new Selda connection for the given backend and database
--   identifier string.
newNeldaConnection :: MonadIO m => Connection -> Text -> m NeldaConnection
newNeldaConnection conn dbid =
  liftIO
    $ NeldaConnection conn dbid
    <$> newIORef M.empty
    <*> newIORef False
    <*> newMVar ()

-- | Get all statements and their corresponding identifiers for the current
--   connection.
allStmts :: NeldaConnection -> IO [(StmtID, Statement)]
allStmts = fmap (map (\(k, v) -> (StmtID k, stmtHandle v)) . M.toList)
  . readIORef
  . connStmts

-- | Close a reusable Selda connection.
--   Closing a connection while in use is undefined.
--   Passing a closed connection to 'runSeldaT' results in a 'SeldaError'
--   being thrown. Closing a connection more than once is a no-op.
closeNeldaConnection :: MonadIO m => NeldaConnection -> m ()
closeNeldaConnection c = liftIO $ mask_ $ do
    closed <- atomicModifyIORef' (connClosed c) $ \closed -> (True, closed)
    unless closed $ do
        stmts <- map snd <$> allStmts c
        closeConnection (connConnection c) stmts
