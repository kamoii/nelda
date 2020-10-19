{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.Schema.ColumnTypeDefs where

import Database.Nelda.Schema.Types (ColumnName)
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
    = RInt
    | RText
    deriving (Show)

{-
data SqlColumnTypeRep where
    TUnsingedInt :: Maybe Int -> SqlColumnTypeRep
    TInt :: SqlColumnTypeRep
    TText :: SqlColumnTypeRep
    TSerial :: SqlColumnTypeRep
-}

data SqlColumnTypeKind
    = TInt
    | TText

-- * SqlColumnType型クラス(独自)

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
-- constraint系のカラムは explicit なものだけ。
-- 型として nullability ~ 'ImplicitNotNull でも constraintNotNull は基本 False
-- 基本 Auto Incremnt と Defawult 両方が入ることはない(と思われるが)..
data Column name columnType sqlType (nullability :: ColumnNull) (default_ :: ColumnDefault) = Column
    { colName :: ColumnName name
    , colType :: ColumnType columnType sqlType
    , constraintNotNull :: Bool
    , constraintAutoIncrement :: Bool
    , constraintDefault :: Maybe Text
    }

deriving instance Show (Column name columnType sqlType nullability default_)

data AnyColumn = forall a b c d e. AnyColumn (Column a b c d e)

deriving instance Show AnyColumn

data Columns (cols :: [*]) = Columns [AnyColumn]
    deriving (Show)

-- * Nullability(共通実装)

data ColumnNull
    = Nullable
    | NotNull
    | ImplicitNotNull
    deriving (Eq, Show)

-- * Default(共通実装)

data ColumnDefault
    = NoDefault
    | AutoIncrement
    | ExplicitDefault
    | ImplicitAutoIncrement
    deriving (Eq, Show)

-- * ColumnType(共通実装)

data ColumnType (ct :: SqlColumnTypeKind) (st :: Type) = ColumnType SqlColumnTypeRep
    deriving (Show)
