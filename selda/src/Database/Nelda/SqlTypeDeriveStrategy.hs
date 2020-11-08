{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.SqlTypeDeriveStrategy where

import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Database.Nelda.SqlType

-- | DB共通の DeriveStrategy for DerivingVia extension

-- | Text 型を BaseSqlType に持つ Enum
-- DB が ENUM 型を持たない場合に利用(Sqliteなど)。
-- PostgreSQL/MySQL のように ENUM型をサポートする場合でも使っても構わない
newtype TextEnum a = TextEnum a
    deriving (Show)

instance (Typeable a, Bounded a, Enum a, Show a, Read a) => SqlType (TextEnum a) where
    type OriginSqlType (TextEnum a) = Text
    toSqlParam (TextEnum e) = toSqlParam $ pack $ show e
    fromSqlValue = TextEnum . read . unpack . fromSqlValue
    toSqlExpression (TextEnum e) = toSqlExpression $ pack $ show e

-- TODO: IntEnum も同様に考えられるはず
-- Int は危険か... 名前と違って変更があった際に値がずれる可能性があるから..
-- UnsafeIntEnum とかにするか？

-- TODO: TextJson もある便利だよね(依存に aeson が必要になっちゃうけど jrec が既にあるから...)
