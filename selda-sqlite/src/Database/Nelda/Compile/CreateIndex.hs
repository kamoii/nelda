{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Nelda.Compile.CreateIndex where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Index(..), IndexName(..))
import Database.Nelda.Compile.Schema (quoteColumnName, quoteTableName)
import qualified Data.Text as Text
import Data.Coerce (coerce)

data Config = Config
    { createIfNotExists :: Bool
    }

-- デフォルトは安全側に倒しておく
defaultConfig :: Config
defaultConfig = Config { createIfNotExists = False }

-- -- | Compile the @CREATE INDEX@ queries for all indexes on the given table.
-- https://sqlite.org/lang_createindex.html
compileCreateIndex :: Config -> Index -> Sql
compileCreateIndex Config{..} Index{..} = statement
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
   ifNotExists = if createIfNotExists then "IF NOT EXISTS " else ""
   columns = Text.intercalate ", " $ map quoteColumnName indexColumns
