{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Database.Nelda.Schema.Table
    ( module Database.Nelda.Schema.Table
    , module Database.Nelda.Schema.Table.Types
    ) where

import Database.Nelda.Schema.Table.Types
import Database.Nelda.Schema.IsColumns
import Database.Nelda.Schema.Column (Columns)

-- table はちょっとした短縮記法。
-- あまりショートカットは用意しないほうがいいが,このケースだと
-- 後ろに & primaryKey (..) のように繋げるに $ columns (...) と書いてしまうと不憫
table'
    :: Tagged TableName name
    -> Columns cols
    -> Table name cols
table' (Tagged tabName) tabColumns = Table
    { tabName
    , tabColumns
    , tabConstraints = []
    , tabIndexies = []
    }

table
    :: (IsColumns columns)
    => Tagged TableName name
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
