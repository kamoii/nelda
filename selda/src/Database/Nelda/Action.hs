module Database.Nelda.Action where

import Database.Selda.Backend.Internal (MonadSelda, SeldaBackend, withBackend, runStmt)
import Database.Selda.Backend.Types (SqlParam)

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)


insert
    :: (MonadSelda m)
    => Table a
    -> [a]
    -> m Int
insert _ [] =
    return 0
insert t cs = withBackend $ \b ->
    sum <$> mapM (uncurry exec) (compileInsert Config.ppConfig t cs)


{-# INLINE _exec #-}
-- | Execute a statement without a result.
_exec :: MonadSelda m => Text -> [SqlParam] -> m Int
_exec q ps = withBackend $ \b -> liftIO $ _execIO b q ps

{-# INLINE _execIO #-}
-- | Like 'exec', but in 'IO'.
_execIO :: SeldaBackend b -> Text -> [SqlParam] -> IO Int
_execIO backend q ps = fmap fst $ runStmt backend q ps
