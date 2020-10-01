{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Database.Nelda.ColType where

import Database.Nelda.SqlColumnType(SqlColumnTypeRep, SqlColumnType(..))
import Database.Nelda.SqlType(SqlType(..))

import Data.Kind (Type)
import Data.Coerce (coerce)

-- * ColType

data ColType (ct :: p) (st :: Type) = ColType SqlColumnTypeRep

-- * _type/asSqlType Helpers

-- _type の使いかたは安全ではない(unsafe preifx 付けるか？)
-- ライブラリを実装する上での注意点だが, SqlColumnTypeRep は ct に対応するものでないといけない。
-- TODO: これ修正するか？
-- いやライブラリだけの安全性を無理して得る必要はないかな。
-- 実装するなら SqlColumnType 型クラスに type class を追加する必要が出てくる
_type :: SqlColumnType ct => SqlColumnTypeRep -> ColType ct (ToSqlType ct)
_type rep = ColType rep

-- | SqlColumnType によって基本 対応する SqlType が決まるが,互換性のあるSqlType に変えたい場合。
-- 互換性のある,というのは ToSqlType ct ~ OriginSqlType st' という条件で確認している。
--
-- 記法については検討の余地あり。
-- 今なら `(asSqlType @Foo unsignedInt)' という形式だが,SqlType にメソッド追加して,
-- `(unsignedInt `as` sqlType @Foo)' のほうが分かりやすいかも...?
--
-- 互換性がない(isomorphicではない) unsafe版も利便性のために用意したほうがいいかも？
asSqlType
    :: forall st' st ct
    . ( SqlType st'
      , SqlColumnType ct
      , ToSqlType ct ~ OriginSqlType st'
      )
    => ColType ct st
    -> ColType ct st'
asSqlType = coerce
