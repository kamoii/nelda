{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Nelda.Backend.Monad where

import Control.Monad.IO.Class (MonadIO)
import Database.Nelda.Backend.Connection (connConnection, connClosed, connLock, NeldaConnection)
import Control.Monad.State (runStateT, when, get, StateT)
import Control.Monad.Catch (throwM, bracket, MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (putMVar, takeMVar)
import Data.IORef (readIORef)
import Database.Nelda.Backend.Types (Connection, BackendError(DbError))

-- | Some monad with Nelda SQL capabilitites.
class MonadIO m => MonadNelda m where
    {-# MINIMAL withNeldaConnection #-}

    -- | Pass a Nelda connection to the given computation and execute it.
    --   After the computation finishes, @withConnection@ is free to do anything
    --   it likes to the connection, including closing it or giving it to another
    --   Nelda computation.
    --   Thus, the computation must take care never to return or otherwise
    --   access the connection after returning.
    withNeldaConnection :: (NeldaConnection -> m a) -> m a

    -- | Perform the given computation as a transaction.
    --   Implementations must ensure that subsequent calls to 'withConnection'
    --   within the same transaction always passes the same connection
    --   to its argument.
    transact :: m a -> m a
    transact = id

withConnection :: MonadNelda m => (Connection -> m a) -> m a
withConnection cb =
    withNeldaConnection (cb . connConnection)

-- | Monad transformer adding Nelda SQL capabilities.
newtype NeldaT m a = S {unS :: StateT NeldaConnection m a}
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadThrow, MonadCatch, MonadMask , MonadFail
             )

instance (MonadIO m, MonadMask m) => MonadNelda (NeldaT m) where
    withNeldaConnection m = S get >>= m

instance MonadTrans NeldaT where
    lift = S . lift

-- | The simplest form of Nelda computation; 'NeldaT' specialized to 'IO'.
type NeldaM = NeldaT IO

-- | Run a Nelda transformer. Backends should use this to implement their
--   @withX@ functions.
runNeldaT
    :: (MonadIO m, MonadMask m)
    => NeldaT m a
    -> NeldaConnection
    -> m a
runNeldaT m c =
    bracket (liftIO $ takeMVar (connLock c))
            (const $ liftIO $ putMVar (connLock c) ())
            (const go)
  where
    go = do
        closed <- liftIO $ readIORef (connClosed c)
        when closed $ do
            liftIO $ throwM $ DbError "runNeldaT called with a closed connection"
        fst <$> runStateT (unS m) c
