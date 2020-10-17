{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Nelda.Action where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Table(..))
import Database.Nelda.Compile.Insert (InsertableTable, InsertableTable', InsertRecordFields, compileInsert, compileInsert')
import Database.Nelda.Backend.Types (SqlParam)

import Database.Selda.Backend.Internal (MonadSelda, SeldaBackend, withBackend, runStmt)

import Control.Monad.IO.Class (liftIO)

import JRec hiding (insert)
import Data.Functor (void)


insert
    :: (MonadSelda m, InsertableTable (Table name cols) lts)
    => Table name cols
    -> [Rec lts]
    -> m Int
insert _ [] =
    return 0
insert t cs =
    sum <$> mapM (uncurry _exec) (compileInsert t cs)

insert_
    :: (MonadSelda m, InsertableTable (Table name cols) lts)
    => Table name cols
    -> [Rec lts]
    -> m ()
insert_ t cs = void $ insert t cs

insert'
    :: (MonadSelda m, InsertableTable' (Table name cols))
    => Table name cols
    -> [Rec (InsertRecordFields (Table name cols))]
    -> m Int
insert' _ [] =
    return 0
insert' t cs =
    sum <$> mapM (uncurry _exec) (compileInsert' t cs)


{-# INLINE _exec #-}
-- | Execute a statement without a result.
_exec :: MonadSelda m => Sql -> [SqlParam] -> m Int
_exec q ps = withBackend $ \b -> liftIO $ _execIO b q ps

{-# INLINE _execIO #-}
-- | Like 'exec', but in 'IO'.
_execIO :: SeldaBackend b -> Sql -> [SqlParam] -> IO Int
_execIO backend (Sql sql) ps = fmap fst $ runStmt backend sql ps
