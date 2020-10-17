{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Database.Nelda.Schema.Table
    ( module Database.Nelda.Schema.Table
    , module Database.Nelda.Schema.TableTypeDefs
    ) where

import Database.Nelda.Schema.Types (TableName)
import Database.Nelda.Schema.ColumnTypeDefs (Columns)
import Database.Nelda.Schema.TableTypeDefs
import Database.Nelda.Schema.IsColumns

-- table はちょっとした短縮記法。
-- あまりショートカットは用意しないほうがいいが,このケースだと
-- 後ろに & primaryKey (..) のように繋げるに $ columns (...) と書いてしまうと不憫
table'
    :: TableName name
    -> Columns cols
    -> Table name cols
table' tabName tabColumns = Table
    { tabName
    , tabColumns
    , tabAttrs = []
    }

table
    :: (IsColumns columns)
    => TableName name
    -> columns
    -> Table name (ToColumnsType columns)
table tabName = table' tabName . columns


-- sample
-- fooTable :: _
-- fooTable =
--     table #foo
--         ( column #id   unsignedInt & notNull
--         , column #name text
--         , column #age  unsignedInt
--         )
--     & primaryKey (#name, #age)
