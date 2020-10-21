{-# LANGUAGE NamedFieldPuns #-}

module Database.Nelda.Schema.Index where

import Database.Nelda.Schema.IsColumnNames (IsColumnNames(..))
import Database.Nelda.Schema.IsColumnSubset (IsColumnSubset)
import Database.Nelda.Schema.Table (Table(..))


-- index
--     :: ( IsColumnNames colNames
--       , IsColumnSubset name cols (ToColumnNamesType colNames)
--       )
--     => colNames
--     -> Table name cols
--     -> Table name cols
-- index ts table@(Table {tabAttrs}) =
--     table { tabAttrs = PrimaryKey (toAnyColumnNames ts) : tabAttrs }



-- TODO: uniqueIndex
