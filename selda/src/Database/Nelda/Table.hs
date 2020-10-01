{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Database.Nelda.Table where

import Database.Nelda.Types (TableName)
import Database.Nelda.TableAttr
import Database.Nelda.IsColumns

import GHC.TypeLits as TL (Symbol)

-- * Table

{-
table 作成時に column の非重複チェック。
重複チェックは結構面倒なので後回し
主キーの設定かな？
table の型は PartialSignature を許しておくのがいい感じかも(schema の変更毎に変わるのはうっとしいので)
2020/09/28 現状は primary key は型レベルでは情報を持たない

table 関数は columns と primaryKeys までかな取りあえず。
table columsn prims & addINdex ... & addUniqIndex ...

constraint と index は別にする必要あるかな
-}
data Table (name :: Symbol) (cols :: [*]) = Table
    { tabName :: TableName name
    , tabColumns :: Columns cols
    , tabAttrs :: [TableAttr]
    -- ^ Table level Constraints/Attributes
    }

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
