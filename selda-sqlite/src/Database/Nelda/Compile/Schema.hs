{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Database.Nelda.Compile.Schema where

import Database.Nelda.Schema.Types (TableName(..), ColumnName(..))
import Database.Nelda.SqlType (SqlType(toSqlExpression))
import Database.Nelda.Schema.ColumnTypeDefs (SqlColumnTypeRep(..), Column(..), ColumnType(..), ColumnNull(..), ColumnDefault(..))
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text

-- 参照
-- https://sqlite.org/lang_createtable.html

compileColumn :: Column name columnType sqlType nullability default_ -> Text
compileColumn Column{..} = Text.unwords $ [ name, type_ ] <> constraints
  where
    name = quoteColumnName colName
    type_ = compileColumnType colType
    constraints = catMaybes [notNullCons , defaultCons , autoIncCons ]
    notNullCons = if constraintNotNull then Just "NOT NULL" else Nothing
    defaultCons = ("DEFAULT " <>) <$> constraintDefault
    autoIncCons = if constraintNotNull then Just "AUTOINCREMENT" else Nothing

compileColumnType :: ColumnType ct st -> Text
compileColumnType (ColumnType rep) =
    case rep of
        RInt -> "INTEGER"
        RText -> "TEXT"

-- * Quoting

quoteTableName :: TableName a -> Text
quoteTableName (TableName name) = mconcat ["\"", _escapeQuotes name, "\""]

quoteColumnName :: ColumnName a -> Text
quoteColumnName (ColumnName name) = mconcat ["\"", _escapeQuotes name, "\""]

_escapeQuotes = Text.replace "\"" "\"\""
