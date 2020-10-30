{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Action where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Proxy (Proxy (Proxy))
import Database.Nelda.Backend.Monad (MonadNelda, withConnection)
import Database.Nelda.Backend.Runner (runStmt)
import Database.Nelda.Backend.Types (Connection, SqlParam, SqlValue)
import qualified Database.Nelda.Compile.Delete as Delete
import qualified Database.Nelda.Compile.Index as Index
import Database.Nelda.Compile.Insert (InsertRecordFields, InsertableTable, InsertableTable', compileInsert, compileInsert')
import qualified Database.Nelda.Compile.Query as Query
import qualified Database.Nelda.Compile.Table as Table
import Database.Nelda.Compile.TableFields (ToQueryFields)
import Database.Nelda.Compile.Types
import Database.Nelda.Query.Monad (Query)
import Database.Nelda.Query.Result (Res, Result, buildResult)
import Database.Nelda.SQL.Col (Col)
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Types (paramToSqlParam)
import Database.Nelda.Schema (Table (..))
import Database.Nelda.Types (Sql (..))
import GHC.Generics (Generic (Rep))
import JRec hiding (insert)
import qualified JRec.Internal as JRec

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
query ::
    forall m a s.
    (MonadNelda m, Result a) =>
    Query s a ->
    m [Res a]
query q = withConnection $ \conn -> do
    let (sql, params) = Query.compileQuery q
    res <- liftIO $ runStmt conn sql (map paramToSqlParam params)
    pure $ mkResults (Proxy :: Proxy a) (snd res)
  where
    mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
    mkResults p = map (buildResult p)

-- * INSERT

insert ::
    (MonadNelda m, InsertableTable (Table name cols) lts) =>
    Table name cols ->
    [Rec lts] ->
    m Int
insert _ [] =
    return 0
insert t cs =
    sum <$> mapM (uncurry _exec) (compileInsert t cs)

insert_ ::
    (MonadNelda m, InsertableTable (Table name cols) lts) =>
    Table name cols ->
    [Rec lts] ->
    m ()
insert_ t cs = void $ insert t cs

insertFromNative_ ::
    ( MonadNelda m
    , Generic a
    , JRec.FromNative (Rep a) lts
    , InsertableTable (Table name cols) lts
    ) =>
    Table name cols ->
    [a] ->
    m ()
insertFromNative_ t cs = insert_ t $ map JRec.fromNative cs

insert' ::
    (MonadNelda m, InsertableTable' (Table name cols)) =>
    Table name cols ->
    [Rec (InsertRecordFields (Table name cols))] ->
    m Int
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
-- * DELETE

-- | From the given table, delete all rows matching the given predicate.
--   Returns the number of deleted rows.
deleteFrom ::
    (MonadNelda m, lts ~ ToQueryFields cols) =>
    Table name cols ->
    (Row s (Rec lts) -> Col s Bool) ->
    m Int
deleteFrom tbl predicate =
    uncurry _exec $ Delete.compileDelete tbl predicate

-- | Like 'deleteFrom', but does not return the number of deleted rows.
deleteFrom_ ::
    (MonadNelda f, lts ~ ToQueryFields cols) =>
    Table name cols ->
    (Row s (Rec lts) -> Col s Bool) ->
    f ()
deleteFrom_ tbl predicate = void $ deleteFrom tbl predicate

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
