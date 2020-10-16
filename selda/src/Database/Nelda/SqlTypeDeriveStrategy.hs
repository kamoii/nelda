{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Nelda.SqlTypeDeriveStrategy where

import Database.Nelda.SqlType
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)

-- | DB共通の DeriveStrategy for DerivingVia extension

-- | Text 型を BaseSqlType に持つ Enum
-- DB が ENUM 型を持たない場合に利用(Sqliteなど)。
-- PostgreSQL/MySQL のように ENUM型をサポートする場合でも使っても構わない

newtype TextEnum a = TextEnum a
    deriving (Show)

instance (Typeable a, Bounded a, Enum a, Show a, Read a) => SqlType (TextEnum a) where
    type OriginSqlType (TextEnum a) = Text
    sqlTypeRep = sqlTypeRep @Text
    toSqlParam (TextEnum e) = toSqlParam $ pack $ show e
    fromSqlValue = TextEnum . read . unpack . fromSqlValue
    defaultValue = TextEnum minBound


-- TODO: IntEnum も同様に考えられるはず
