{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Nelda.Action where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Table(..))
import Database.Nelda.Compile.Insert (InsertableTable, InsertableTable', InsertRecordFields, compileInsert, compileInsert')
import qualified Database.Nelda.Compile.Table as Table
import qualified Database.Nelda.Compile.Index as Index
import Database.Nelda.Backend.Types (SqlParam)

import Database.Selda.Backend.Internal (MonadSelda, SeldaBackend, withBackend, runStmt)

import Control.Monad.IO.Class (liftIO)

import JRec hiding (insert)
import Data.Functor (void)
import Database.Nelda.Compile.Types

-- * INSERT

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

-- * CREATE TABLE/CREATE INDEX

-- | Create a table from the given schema.
createTable :: MonadSelda m => Table name cols -> m ()
createTable tbl = do
  createTableWithoutIndexes IgnoreExistence tbl
  createTableIndexes IgnoreExistence tbl

createTableIfNotExists :: MonadSelda m => Table name cols -> m ()
createTableIfNotExists tbl = do
  createTableWithoutIndexes ConcernExistence tbl
  createTableIndexes ConcernExistence tbl

-- | Create a table from the given schema, but don't create any indexes.
createTableWithoutIndexes :: MonadSelda m => ExistenceCheck -> Table name cols -> m ()
createTableWithoutIndexes ec tbl = withBackend $ \_b -> do
  void $ _exec (Table.compileCreateTable ec tbl) []

-- -- | Create all indexes for the given table. Fails if any of the table's indexes
-- --   already exists.
createTableIndexes :: MonadSelda m => ExistenceCheck -> Table name cols -> m ()
createTableIndexes ec Table{tabIndexies} = withBackend $ \_b -> do
  mapM_ (flip _exec [] . Index.compileCreateIndex ec) tabIndexies

-- * DROP TABLE

-- SQLite ではテーブルを削除すると index も削除される。PostgreSQL/MySQLでも多分同様。
-- 参照 https://sqlite.org/lang_droptable.html

-- | Create a table from the given schema.
dropTable :: MonadSelda m => Table name cols -> m ()
dropTable tbl = withBackend $ \_b -> do
  void $ _exec (Table.compileDropTable IgnoreExistence tbl) []

dropTableIfExists :: MonadSelda m => Table name cols -> m ()
dropTableIfExists tbl = withBackend $ \_b -> do
  void $ _exec (Table.compileDropTable ConcernExistence tbl) []

-- * Executer

{-# INLINE _exec #-}
-- | Execute a statement without a result.
_exec :: MonadSelda m => Sql -> [SqlParam] -> m Int
_exec q ps = withBackend $ \b -> liftIO $ _execIO b q ps

{-# INLINE _execIO #-}
-- | Like 'exec', but in 'IO'.
_execIO :: SeldaBackend b -> Sql -> [SqlParam] -> IO Int
_execIO backend (Sql sql) ps = fmap fst $ runStmt backend sql ps
