{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Nelda.Action where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Table(..))
import Database.Nelda.Compile.Insert (InsertableTable, InsertableTable', InsertRecordFields, compileInsert, compileInsert')
import qualified Database.Nelda.Compile.Table as Table
import qualified Database.Nelda.Compile.Index as Index
import qualified Database.Nelda.Compile.Query as Query
import Database.Nelda.Compile.Types
import Database.Nelda.Query.Result (buildResult, Res, Result)
import Database.Nelda.Query.Monad (Query)
import Database.Nelda.Backend.Types (Connection, SqlParam, SqlValue)
import Database.Nelda.SQL.Types (paramToSqlParam)
import Database.Nelda.Backend.Runner (runStmt)
import Database.Nelda.Backend.Monad (withConnection, MonadNelda)

import JRec hiding (insert)
import Data.Functor (void)
import Data.Proxy (Proxy(Proxy))
import Control.Monad.IO.Class (liftIO)

-- * SELECT QUERY

-- | Run a query within a Nelda monad. In practice, this is often a 'NeldaT'
--   transformer on top of some other monad.
--   Nelda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
-- query :: (MonadNelda m, Result a) => Query s a -> m [Res a]
-- query q = withConnection (flip queryWith q . runStmt')
--   where
--     runStmt' b = \sql params -> runStmt b sql (map paramToSqlParam params)

-- TODO: Support queryInto
-- | Perform the given query, and insert the result into the given table.
--   Returns the number of inserted rows.
-- queryInto
--     :: (MonadNelda m, Relational a)
--     => Table a
--     -> Query (Backend m) (Row (Backend m) a)
--     -> m Int
-- queryInto tbl q = withBackend $ \b -> do
--     let (qry, ps) = compileWith Config.ppConfig q
--         qry' = mconcat ["INSERT INTO ", tblName, " ", qry]
--     fmap fst . liftIO $ runStmt b qry' (map paramToParam ps)
--   where
--     tblName = fromTableName (tableName tbl)

-- | Build the final result from a list of result columns.
query
    :: forall m a s. (MonadNelda m, Result a)
    => Query s a
    -> m [Res a]
query q = withConnection $ \conn -> do
    let (sql, params) = Query.compileQuery q
    res <- liftIO $ runStmt conn sql (map paramToSqlParam params)
    pure $ mkResults (Proxy :: Proxy a) (snd res)
  where
    -- | Generate the final result of a query from a list of untyped result rows.
    mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
    mkResults p = map (buildResult p)

-- * INSERT

insert
    :: (MonadNelda m, InsertableTable (Table name cols) lts)
    => Table name cols
    -> [Rec lts]
    -> m Int
insert _ [] =
    return 0
insert t cs =
    sum <$> mapM (uncurry _exec) (compileInsert t cs)

insert_
    :: (MonadNelda m, InsertableTable (Table name cols) lts)
    => Table name cols
    -> [Rec lts]
    -> m ()
insert_ t cs = void $ insert t cs

insert'
    :: (MonadNelda m, InsertableTable' (Table name cols))
    => Table name cols
    -> [Rec (InsertRecordFields (Table name cols))]
    -> m Int
insert' _ [] =
    return 0
insert' t cs =
    sum <$> mapM (uncurry _exec) (compileInsert' t cs)

-- * CREATE TABLE/CREATE INDEX

-- | Create a table from the given schema.
createTable :: MonadNelda m => Table name cols -> m ()
createTable tbl = do
    createTableWithoutIndexes IgnoreExistence tbl
    createTableIndexes IgnoreExistence tbl

createTableIfNotExists :: MonadNelda m => Table name cols -> m ()
createTableIfNotExists tbl = do
    createTableWithoutIndexes ConcernExistence tbl
    createTableIndexes ConcernExistence tbl

-- | Create a table from the given schema, but don't create any indexes.
createTableWithoutIndexes :: MonadNelda m => ExistenceCheck -> Table name cols -> m ()
createTableWithoutIndexes ec tbl =
    void $ _exec (Table.compileCreateTable ec tbl) []

-- -- | Create all indexes for the given table. Fails if any of the table's indexes
-- --   already exists.
createTableIndexes :: MonadNelda m => ExistenceCheck -> Table name cols -> m ()
createTableIndexes ec Table{tabIndexies} =
    mapM_ (flip _exec [] . Index.compileCreateIndex ec) tabIndexies

-- * DROP TABLE

-- SQLite ではテーブルを削除すると index も削除される。PostgreSQL/MySQLでも多分同様。
-- 参照 https://sqlite.org/lang_droptable.html

-- | Create a table from the given schema.
dropTable :: MonadNelda m => Table name cols -> m ()
dropTable tbl =
    void $ _exec (Table.compileDropTable IgnoreExistence tbl) []

dropTableIfExists :: MonadNelda m => Table name cols -> m ()
dropTableIfExists tbl =
    void $ _exec (Table.compileDropTable ConcernExistence tbl) []

-- * Executer

{-# INLINE _exec #-}
-- | Execute a statement without a result.
_exec :: MonadNelda m => Sql -> [SqlParam] -> m Int
_exec sql ps = withConnection $ \conn -> liftIO $ _execIO conn sql ps

{-# INLINE _execIO #-}
-- | Like 'exec', but in 'IO'.
_execIO :: Connection -> Sql -> [SqlParam] -> IO Int
_execIO conn sql ps = fmap fst $ runStmt conn sql ps
