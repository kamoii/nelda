{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Database.Nelda.Schema.SqlTableType where

import Database.Nelda.Schema.Types (TableName, AnyColumnName)
import Database.Nelda.Schema.SqlColumnType (Columns)

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
    } deriving (Show)

data TableAttr
    = PrimaryKey [AnyColumnName]
    deriving (Show)
