{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Database.Nelda.Compile.TableFields where

import Database.Nelda.Schema (ColumnDefault(..), ColumnNull(..), Column)
import qualified JRec

-- | テーブル定義(カラム定義)から Rec lts の lts を決定するための type families
--
-- ライブラリが提供する関数から作成された columns :: [*] であればエラーにはならないはず。

-- * Query Fields/Types
--
-- select する場合や, delete from の where 句の中など

type family ToQueryFields columns :: [*] where
    ToQueryFields '[] = '[]
    ToQueryFields (column ': cs) = (ToQueryField column ': ToQueryFields cs)

type family ToQueryField column :: * where
    ToQueryField (Column name _ sqlType nullabilty _ _) =
        name JRec.:= QueryTypeColumnNullWrapping nullabilty sqlType

type family QueryTypeColumnNullWrapping (nullabilty :: ColumnNull) (target :: *) :: * where
    QueryTypeColumnNullWrapping 'NotNull t = t
    QueryTypeColumnNullWrapping 'Nullable t = Maybe t
    QueryTypeColumnNullWrapping 'ImplicitNotNull t = t

-- * Insert Fields/Types
--
-- insert, update? で利用

-- Data type to specify columns which has explicit DEFAULT value or AUTO INCREMENT attribute.
-- いずれも Maybe 型と isomorphic だが semantics が異なるための別の型として定義している。
-- 現状 一つのカラムが explicit DEFAULT value と AUTO INCREMENT attribute を同時に持つことはないと仮定している。
-- つまり AutoIncrement (Defaultable Int) みたいなINSERT型はないはず(ToInsertRecordFieldの実装参照)。
-- そのため一つのデータ型で済むはずではあるが,性質を変えて扱いたい場合があるので別々に定義。
--
-- InsertableTable 型クラスを使う限り,ユーザがこれらの型を使うことは稀のはずである。
-- なので型名やコンストラクタが長ったらしくなってもまーいいかな。
--
-- TODO: 定義場所正しいのか？
-- TODO: Defaultable is an awful name..
data Defaultable a
    = UseDefault
    | IgnoreDefaultAndSpecify a
    deriving (Eq, Show)

data AutoIncrement a
    = TriggerAutoIncrement
    | IgnoreAutIncrementAndSpecify a
    deriving (Eq, Show)

type family ToInsertFields columns :: [*] where
    ToInsertFields '[] = '[]
    ToInsertFields (column ': cs) = (ToInsertField column ': ToInsertFields cs)

type family ToInsertField column :: * where
    ToInsertField (Column name _ sqlType nullabilty default_ _) =
        name JRec.:= InsertTypeColumnDefaultWrapping default_ (InsertTypeColumnNullWrapping nullabilty sqlType)

type family InsertTypeColumnNullWrapping (nullabilty :: ColumnNull) (target :: *) :: * where
    InsertTypeColumnNullWrapping 'NotNull t = t
    InsertTypeColumnNullWrapping 'Nullable t = Maybe t
    InsertTypeColumnNullWrapping 'ImplicitNotNull t = t

type family InsertTypeColumnDefaultWrapping (default_ :: ColumnDefault) (target :: *) :: * where
    InsertTypeColumnDefaultWrapping 'NoDefault t = t
    InsertTypeColumnDefaultWrapping 'AutoIncrement t = AutoIncrement t
    InsertTypeColumnDefaultWrapping 'ExplicitDefault t = Defaultable t
    InsertTypeColumnDefaultWrapping 'ImplicitAutoIncrement t = AutoIncrement t
