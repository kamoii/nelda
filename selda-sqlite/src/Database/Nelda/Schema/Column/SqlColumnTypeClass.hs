{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Database.Nelda.Schema.Column.SqlColumnTypeClass where

import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind ( SqlColumnTypeKind )
import Database.Nelda.SqlType (SqlType)
import Database.Nelda.Schema.Column.Types (ColumnDefault(..), ColumnNull(..))

-- !! SqlColumnType 型クラスは SqlColumnTypeRep を返すメソッドを定義できない。
-- !! 厄介なのは  型によっては optional なパラメータを取るということ(MySQL, SQLite, Postgres共に)
-- !! そのため カラム定義する際に
class (SqlType (ToSqlType ct)) => SqlColumnType (ct :: SqlColumnTypeKind) where
    type ToSqlType ct

    -- ほぼ全ての型において NULL許容/NO DEFAULT なのだが,
    -- PostgreSQLの SERIAL系の pseudo-type だけが暗黙的に NOT NULL/DEFAULT が必要であるため導入された。
    -- なので上記以外の型はデフォルト実装を使えばいいはずなので不要。
    --
    -- NOTE: ct の type application が失敗するのでしょうがないので Proxy 導入で。
    -- NOTE: 型チェック通すためにややこしいことしている。今のところ PostgreSQL の SERIAL系(三つの型のみ)のためだけ。
    -- まあ以下の記述以外に複雑なことはないから大丈夫かな？複雑さが漏れるようなら諦めたほうがいいかも

    --
    -- ImplicitNotNull か Nullable のいずれかになるはず。
    -- NotNull は明示的な NOT NULL 制約にのみ付くので。
    type InitialNullability ct :: ColumnNull
    type InitialNullability _ct = 'Nullable
    -- initialNullability :: Proxy ct -> ColumnNull (InitialNullability ct)

    type InitialDefault ct :: ColumnDefault
    type InitialDefault _ct = 'NoDefault
    -- initialDefault :: Proxy ct -> ColumnDefault (InitialDefault ct)
