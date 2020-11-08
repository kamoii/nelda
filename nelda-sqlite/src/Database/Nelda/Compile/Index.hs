{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Nelda.Compile.Index where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Index(..), IndexName(..))
import Database.Nelda.Compile.Types
import qualified Data.Text as Text
import Data.Coerce (coerce)
import Database.Nelda.Compile.Quoting (quoteTableName, quoteColumnName)

-- -- | Compile the @CREATE INDEX@ queries for all indexes on the given table.
-- https://sqlite.org/lang_createindex.html
compileCreateIndex :: ExistenceCheck -> Index -> Sql
compileCreateIndex ec Index{..} = statement
 where
   statement = Sql $ mconcat
       [ "CREATE ", unique, "INDEX ", ifNotExists
       , indexName'
       , " ON ", tableName
       , " (", columns, ")"
       ]
   unique = if indexIsUnique then "UNIQUE " else ""
   tableName = quoteTableName indexTableName
   indexName' = coerce indexName
   ifNotExists = if ec == ConcernExistence then "IF NOT EXISTS " else ""
   columns = Text.intercalate ", " $ map quoteColumnName indexColumns
