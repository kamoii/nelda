{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Database.Nelda.Compile.Schema where

import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind
import Database.Nelda.Schema.Column
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Database.Nelda.Compile.Quoting (quoteColumnName)

-- 参照
-- https://sqlite.org/lang_createtable.html

compileColumn :: Column name columnType sqlType nullability default_ isPrimary -> Text
compileColumn Column{..} = Text.unwords $ [ name, type_ ] <> constraints0 <> constraints1
  where
    name = quoteColumnName colName
    type_ = compileColumnType colType
    constraints0 = catMaybes
        [ notNullCons
        , defaultCons
        , autoIncCons
        ]
    notNullCons = if constraintNotNull then Just "NOT NULL" else Nothing
    defaultCons = ("DEFAULT " <>) <$> constraintDefault
    autoIncCons = if constraintAutoIncrement then Just "AUTOINCREMENT" else Nothing
    constraints1 = map compileConstraint constraints

compileConstraint :: ColumnConstraint -> Text
compileConstraint CCPrimaryKey = "PRIMARY KEY"
compileConstraint CCUnique = "UNIQUE"

compileColumnType :: ColumnType ct st -> Text
compileColumnType (ColumnType rep) = compileSqlColumnTypeRep rep

compileSqlColumnTypeRep ::  SqlColumnTypeRep -> Text
compileSqlColumnTypeRep rep =
    case rep of
        RInt -> "INTEGER"
        RText -> "TEXT"
        RDouble -> "DOUBLE"
        RBoolean -> "BOOLEAN"
