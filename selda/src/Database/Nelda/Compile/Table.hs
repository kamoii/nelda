{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Nelda.Compile.Table where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Table(..), Columns(..), AnyColumn(..))
import Database.Nelda.Compile.Schema (compileColumn)
import Database.Nelda.Compile.Quoting (quoteTableName)
import qualified Data.Text as Text
import Database.Nelda.Compile.Types

-- * CREATE TABLE
--
-- selda は CreateTable 時に実行時にテーブルの validation を行う。
-- Database.Selda.Table.Validation モジュールで定義されている。
-- チェック項目は以下。Nelda では型レベルでコンパイル時に可能なので不要。
--
--  * テーブル名がnon-空文字列
--  * カラム名がnon-空文字列
--  * 名に \NUL がないか
--  * カラム名の重複
--  * Primary Key の複数"回"指定
--  * "column is used as a foreign key, but is not primary or unique: "??
--
-- DDL statement 中では bound parameter は使えない
-- SQLite3(https://www.sqlite.org/lang_createtable.html) 3.2. The DEFAULT clause
-- PostgreSQL(https://docs.sqlalchemy.org/en/13/core/ddl.html) 多分公式ではないが...
--  ... SQL bind parameters are not available in DDL statements.

-- | Compile a sequence of queries to create the given table, including indexes.
--   The first query in the sequence is always @CREATE TABLE@.
compileCreateTable :: ExistenceCheck -> Table name cols -> Sql
compileCreateTable ec Table{tabName, tabColumns} = statement
  where
    statement = Sql $ mconcat
        [ "CREATE TABLE ", ifNotExists, tableName
        , "("
        , Text.intercalate ", " columnDefinitions
        , ")"
        ]

    ifNotExists        = if ec == ConcernExistence then "IF NOT EXISTS " else ""
    tableName          = quoteTableName tabName
    columnDefinitions  = map (\(AnyColumn column) -> compileColumn column) anyColumns
    Columns anyColumns = tabColumns

    -- multiPrimary =
    --     [ mconcat ["PRIMARY KEY(", intercalate ", " (colNames ixs), ")"]
    --     | (ixs, Primary) <- tableAttrs tbl
    --     ]

    -- multiUniques =
    --     [ mconcat ["UNIQUE(", intercalate ", " (colNames ixs), ")"]
    --     | (ixs, Unique) <- tableAttrs tbl
    --     ]

    -- colNames ixs = [fromColName (colName (tableCols tbl !! ix)) | ix <- ixs]
    -- allFKs = [(colName ci, fk) | ci <- tableCols tbl, fk <- colFKs ci]
    -- compFKs = zipWith (uncurry compileFK) allFKs [0..]

-- -- | Compile a foreign key constraint.
-- compileFK :: ColName -> (Table (), ColName) -> Int -> Text
-- compileFK col (Table ftbl _ _ _, fcol) n = mconcat
--   [ "CONSTRAINT ", fkName, " FOREIGN KEY (", fromColName col, ") "
--   , "REFERENCES ", fromTableName ftbl, "(", fromColName fcol, ")"
--   ]
--   where
--     fkName = fromColName $ addColPrefix col ("fk" <> pack (show n) <> "_")

-- * DROP TABLE

compileDropTable :: ExistenceCheck -> Table name cols -> Sql
compileDropTable ec Table{tabName} = statement
  where
    statement = Sql $ mconcat ["DROP TABLE ", ifExists, tableName]
    ifExists = if ec == ConcernExistence then "IF EXISTS " else ""
    tableName = quoteTableName tabName
