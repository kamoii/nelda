{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Database.Nelda.Schema.Column
    ( module Database.Nelda.Schema.Column
    , module Database.Nelda.Schema.SqlColumnType
    ) where

import Database.Nelda.Schema.Types
import Database.Nelda.Schema.SqlColumnType
import Database.Nelda.SqlType

import Data.Data (Proxy(Proxy))
import Data.Coerce (coerce)


-- | 改良版 column
--
-- ColNull や ColDefault を必須の引数として取らないようにした。
-- PostgreSQL の SERIAL のような pseudo-type は暗黙的に NOT NULL/ DEFAULT ... なので
-- 基本指定がなければ NULL許容(PosgreSQL/MySQL(?)/SQLite(?)) でデフォルト値なし。
-- また殆どのケースで NULL許容/NO DEFAULT のはずなので。
-- 柔軟性は高いかな。
--
-- nullability と default_ の初期値は SqlColumnType が定める。
-- ただしほぼ同じ(NULL OK/NO DEFAULT)なのでデフォルト実装を持てばいい

column
    :: forall st s ct name columnType sqlType nullability default_.
       ( SqlColumnType ct
       , SqlType st
       , name ~ s
       , columnType ~ ct
       , sqlType ~ st
       , nullability ~ InitialNullability ct
       , default_ ~ InitialDefault ct
       )
    => ColumnName s
    -> ColumnType ct st
    -> Column name columnType sqlType nullability default_
column colName colType =
    Column { colName
           , colType
           , colNull = initialNullability (Proxy @ct)
           , colDefault = initialDefault (Proxy @ct)
           }

-- TODO: これは Nullable じゃないときに notNull しようとした場合のエラーメッセージを改善できるかな...
notNull
    :: Column _name _columnType _sqlType 'Nullable _default
    -> Column _name _columnType _sqlType 'NotNull _default
notNull c = c { colNull = CNotNull }

-- TODO: エラーメッセージの改善
-- TODO: defualt_ という名前のほうがいいかな？
-- TODO: 関数呼びだし系の DEAFULT もあるので unsafeDefault も必要かな？
withDefault
    :: SqlColumnType columnType
    => ToSqlType columnType
    -> Column _name columnType _sqlType _nullability 'NoDefault
    -> Column _name columnType _sqlType _nullability 'HasDefault
withDefault v c = c { colDefault = CDefaultBySqlValue v }

-- | SqlColumnType によって基本 対応する SqlType が決まるが,互換性のあるSqlType に変えたい場合。
-- 互換性のある,というのは ToSqlType ct ~ OriginSqlType st' という条件で確認している。
--
-- 記法については検討の余地あり。
-- 今なら `(asSqlType @Foo unsignedInt)' という形式だが,SqlType にメソッド追加して,
-- `(unsignedInt & asSqlType @Foo)' のほうが分かりやすいかも...?
--
-- 互換性がない(isomorphicではない) unsafe版も利便性のために用意したほうがいいかも？
asSqlType
    :: forall st' st ct
    . ( SqlType st'
      , SqlColumnType ct
      , ToSqlType ct ~ OriginSqlType st'
      )
    => ColumnType ct st
    -> ColumnType ct st'
asSqlType = coerce
