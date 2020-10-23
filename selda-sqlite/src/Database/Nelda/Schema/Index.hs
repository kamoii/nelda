{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.Schema.Index where

import Database.Nelda.Schema.IsColumnNames (IsColumnNames(..))
import Database.Nelda.Schema.IsColumnSubset (IsColumnSubset)
import Database.Nelda.Schema.Table (unsafeAddIndex, Table(..))
import Database.Nelda.Schema.Table.Types (Index(..), IndexName)
import Database.Nelda.Schema.Table.Types (TableName(..))
import Database.Nelda.Schema.Column.Types (ColumnName(..))
import qualified Data.Text as Text
import Data.Coerce (coerce)
import Database.Nelda.Schema.Table.Types (IndexName(IndexName))

addIndex
    :: ( IsColumnNames colNames
      , IsColumnSubset name cols (ToColumnNamesType colNames)
      )
    => colNames
    -> Table name cols
    -> Table name cols
addIndex ts table@Table{tabName} = unsafeAddIndex index table
  where
    index = Index
        { indexIsUnique = False
        , indexName = indexNameFor tabName columnNames
        , indexColumns = columnNames
        }
    columnNames = toColumnNames ts

-- | Get the name to use for an index on the given column(s) in the given table.
--
-- To ensure uniqueness
--
-- 1. Name multi-column indexes by connecting column names
--    with underscores.
-- 2. Escape underscores in column names.
--
-- Thus the index of columns @["foo","bar"]@ becomes @ixTable_foo_bar@ while
-- the index @["foo_bar"]@ receives an extra underscore to become
-- @ixTable_foo__bar@.
indexNameFor :: TableName -> [ColumnName] -> IndexName
indexNameFor tn cns = IndexName
    $ "ix"
    <> coerce tn
    <> "_"
    <> Text.intercalate "_" (map (Text.replace "_" "__" . coerce) cns)
