{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Nelda.Schema.SqlColumnType where

import Database.Nelda.Schema.Types (ColumnName)
import Database.Nelda.Types (SqlFragment(..))
import Database.Nelda.SqlType (SqlType)
import Data.Kind (Type)
import Data.Data (Proxy)
import Data.Text (Text)

-- * SqlColumnTypeRep(独自)

-- DBが提供する型
-- ただ INT と INT UNSINGED は別にしないといけない(MySQL上では単一の型だけど以下の理由で分ける必要がある)
-- 対応する haskell type が一意に決まる必要がある。
-- haskell type は単独で backend library の parameter や 結果から一意的に変換できる必要あり。
-- つまり Posgres だと oid に対応している必要あり
--
-- 以下の gadts 的にどうなんだ？？？
data SqlColumnTypeRep
    = TInt
    | TText

{-
data SqlColumnTypeRep where
    TUnsingedInt :: Maybe Int -> SqlColumnTypeRep
    TInt :: SqlColumnTypeRep
    TText :: SqlColumnTypeRep
    TSerial :: SqlColumnTypeRep
-}

-- * SqlColumnType型クラス(独自)

-- !! SqlColumnType 型クラスは SqlColumnTypeRep を返すメソッドを定義できない。
-- !! 厄介なのは  型によっては optional なパラメータを取るということ(MySQL, SQLite, Postgres共に)
-- !! そのため カラム定義する際に
class (SqlType (ToSqlType ct)) => SqlColumnType (ct :: p) where
    type ToSqlType ct

    -- ほぼ全ての型において NULL許容/NO DEFAULT なのだが,
    -- PostgreSQLの SERIAL系の pseudo-type だけが暗黙的に NOT NULL/DEFAULT が必要であるため導入された。
    -- なので上記以外の型はデフォルト実装を使えばいいはずなので不要。
    --
    -- NOTE: ct の type application が失敗するのでしょうがないので Proxy 導入で。
    -- NOTE: 型チェック通すためにややこしいことしている。今のところ PostgreSQL の SERIAL系(三つの型のみ)のためだけ。
    -- まあ以下の記述以外に複雑なことはないから大丈夫かな？複雑さが漏れるようなら諦めたほうがいいかも

    -- 残念ながら以下のエラーにより .hsig 内では default method は定義できない
    -- Illegal default method(s) in class definition of SqlColumnType in hsig file
    type InitialNullability ct :: Nullability
    type InitialNullability ct = 'Nullable
    initialNullability :: Proxy ct -> ColumnNull (InitialNullability ct)
    default initialNullability :: (InitialNullability ct ~ 'Nullable) => Proxy ct -> ColumnNull (InitialNullability ct)
    initialNullability _ = CNullable

    type InitialDefault ct :: Default
    type InitialDefault ct = 'NoDefault
    initialDefault :: Proxy ct -> ColumnDefault ct (InitialDefault ct)
    default initialDefault :: (InitialDefault ct ~ 'NoDefault) => Proxy ct -> ColumnDefault ct (InitialDefault ct)
    initialDefault _ = CNoDefault

instance SqlColumnType 'TInt where
    type ToSqlType 'TInt = Int

instance SqlColumnType 'TText where
    type ToSqlType 'TText = Text


-- * Column型(共通)

{-
 DEFAULT + NULL + AUTO_INCREMENT(PostgreSQLの場合SERIAL/BIGSERIAL)
の組合せによって insert時/ query 時の型が変わりうるということ。
あー,primary key でも変わるんだっけ？
?? update 時は
 insertType, updateType, queryType
 updateType =/ queryType になる可能性はないか...

NOTE: sqlType と ColDefault が HasDefault の場合の中の値の型は違う可能性がある。
ただし ToOriginType とは一致するはず
-}
data Column name columnType sqlType nullability default_ = Column
    { colName :: ColumnName name
    , colType :: ColumnType columnType sqlType
    , colNull :: ColumnNull nullability
    , colDefault :: ColumnDefault columnType default_
    }

data AnyColumn = forall a b c d e. AnyColumn (Column a b c d e)

data Columns (cols :: [*]) = Columns [AnyColumn]

-- * Nullability(共通実装)
-- TODO: これって singleton パターンか？
-- TODO: PostgreSQL の pseudo-type のために CImplicitNotNull が必要かも。
data ColumnNull (n :: Nullability) where
    CNotNull :: ColumnNull 'NotNull
    CNullable :: ColumnNull 'Nullable

data Nullability
    = NotNull
    | Nullable

-- * Default(共通実装)

--- Default 値の値を SqlType だけ取り出すだけで十分かな？
-- TODO: MySQL  の AUTO_INCREMENT のために CAutoIncrement も必要かな？
--
-- NOTE: DEFAULT 値は CREATE文にリテラルとして埋込まずパラメータで渡せるのか？
-- もし渡せないのであれば `CDefault' は TOSqlType ct ではなく,
-- 単に SqlFragment を持てばいいのではないのか？ CREATE文のDEFAULT値を
--
-- またあまりこの型をparameterizeする意味はない気がしてきた...
-- ライブラリユーザが直接この型は触らないので。あくあでライブラリ中の実装が少し安全になる。
data ColumnDefault ct (d :: Default) where
    CNoDefault :: ColumnDefault ct 'NoDefault
    CImplicitDefault :: ColumnDefault ct 'ImplicitDefault
    CDefaultBySqlValue :: SqlColumnType ct => ToSqlType ct -> ColumnDefault ct 'HasDefault
    CDefaultBySqlFragment :: SqlFragment -> ColumnDefault ct 'HasDefault

data Default
    = NoDefault
    | HasDefault
    | ImplicitDefault
      -- ^ For specific context, DEFAULT value is implicitly specified.
      -- For examle PostgreSQL's TINYSERIAL/SERIAL/BIGSERIAL types.
      -- When a type has an implicit default, its normarlly inhibitated to specify explicit default.

-- * Type(共通実装)

data ColumnType (ct :: p) (st :: Type) = ColumnType SqlColumnTypeRep

{-
-- _type の使いかたは安全ではない(unsafe preifx 付けるか？)
-- ライブラリを実装する上での注意点だが, SqlColumnTypeRep は ct に対応するものでないといけない。
-- TODO: これ修正するか？
-- いやライブラリだけの安全性を無理して得る必要はないかな。
-- 実装するなら SqlColumnType 型クラスに type class を追加する必要が出てくる
_type :: SqlColumnType ct => SqlColumnTypeRep -> ColumnType ct (ToSqlType ct)
_type rep = ColumnType rep
-}
