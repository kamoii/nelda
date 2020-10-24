{-# LANGUAGE OverloadedStrings #-}
module Database.Nelda.Compile.Quoting where

import Database.Nelda.Schema (ColumnName(ColumnName), TableName(TableName))
import Data.Text (Text)
import qualified Data.Text as Text

quoteTableName :: TableName -> Text
quoteTableName (TableName name) = mconcat ["\"", _escapeQuotes name, "\""]

quoteColumnName :: ColumnName -> Text
quoteColumnName (ColumnName name) = mconcat ["\"", _escapeQuotes name, "\""]

_escapeQuotes :: Text -> Text
_escapeQuotes = Text.replace "\"" "\"\""
