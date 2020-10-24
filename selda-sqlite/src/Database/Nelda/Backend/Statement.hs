module Database.Nelda.Backend.Statement where

import Database.Nelda.Backend.Types (Statement)
import Database.Nelda.SQL.Types (Param)

import Data.IORef (atomicModifyIORef', newIORef, IORef)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import GHC.IO.Unsafe (unsafePerformIO)

newtype StmtID = StmtID Int

{-# NOINLINE nextStmtId #-}
nextStmtId :: IORef Int
nextStmtId = unsafePerformIO $ newIORef 1

-- | Generate a fresh statement identifier, guaranteed to be unique per process.
freshStmtId :: MonadIO m => m StmtID
freshStmtId = liftIO $ atomicModifyIORef' nextStmtId $ \n -> (n+1, StmtID n)

-- | A prepared statement.
data NeldaStmt = NeldaStmt
    { -- | Backend-specific handle to the prepared statement.
      stmtHandle :: !Statement

    -- | The SQL code for the statement.
    , stmtText :: !Text

    -- | All parameters to be passed to the prepared statement.
    --   Parameters that are unique to each invocation are specified as indices
    --   starting at 0.
    --   Backends implementing @runPrepared@ should probably ignore this field.
    , stmtParams :: ![Either Int Param]
    }
