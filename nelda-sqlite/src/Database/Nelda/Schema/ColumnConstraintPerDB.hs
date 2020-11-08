{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
module Database.Nelda.Schema.ColumnConstraintPerDB where

-- 参照
-- https://sqlite.org/lang_createtable.html

import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind
import Database.Nelda.Schema.Column
import Data.Type.Equality (type (==))
import GHC.TypeLits (TypeError, ErrorMessage(Text))
import Data.Kind (Constraint)

-- * PRIMARY
--
-- SQLite での PRIMARY KEY の扱い
-- https://sqlite.org/lang_createtable.html
--
--   * PRIMARY KEY制約があっても自動的に NOT NULL制約は付かない
--     INTEGER PRIMARY KEY もしくは NOT NULLを明示的に宣言しない限り NULL許容
--     これは SQL Standard とは反する(歴史的な経緯)
--   * INTEGER PRIMARY KEY
--     - シングルカラムが PRIMARY KEY かつ 型が INTEGER かつ WITHOUT ROWID tables でない場合
--     - 特殊な扱いになる
--       - 暗黙的な NOT NULL制約が付く
--       - 暗黙的な AUTOINCREMENT制約が付く
--
-- やらしいのが, table-level constraint で 単一のINTERGERカラムが指定された場合。
-- また WITHOUT ROWID が指定された場合。

primary
    :: ( CheckIfAlreadyPrimary isPrimary
      , isIntegerPrimaryKey ~ (columnType == 'TInt)
      , nullableTo ~ PrimaryNullable isIntegerPrimaryKey nullableFrom
      , defaultTo ~ PrimaryDefault isIntegerPrimaryKey defaultFrom
      )
    => Column _name columnType _sqlType nullableFrom defaultFrom isPrimary
    -> Column _name columnType _sqlType nullableTo   defaultTo   'True
primary = unsafeAddColumnConstraint CCPrimaryKey

-- TODO: primaryWithAutoincrement
-- ただ非推奨ってことはいれるかな？ WARNING pragma

type family CheckIfAlreadyPrimary (isPrimary :: Bool) :: Constraint where
    CheckIfAlreadyPrimary 'True = TypeError ('Text "Already a primary key")
    CheckIfAlreadyPrimary 'False = ()

-- (1)
-- 既に NOT NULL が明示的に与えられているケース。
-- より強い (Epxlicit)NotNull のほうを残す。
-- NOT NULL の重複チェックのためと,あと table-level で WITHOUT ROWID が指定され,
-- INTEGER PRIMARY KEY ではなくなり,暗黙的な NOT NULL を剥すかの判断に必要。
type family PrimaryNullable (isIntegerPrimaryKey :: Bool) (nullableFrom :: ColumnNull) :: ColumnNull where
    PrimaryNullable 'True 'Nullable = 'ImplicitNotNull
    PrimaryNullable 'True 'NotNull = 'NotNull -- (1)
    PrimaryNullable 'True 'ImplicitNotNull = 'ImplicitNotNull
    PrimaryNullable 'False nullable = nullable

-- (1)
-- SQLite で (暗黙,明示)AUTOINCREMENT になるのは PrimaryKey のみ。
-- なので defaultFrom が既に AutoIncrement/ImplicitAutoIncrement なのは既に primary を使った場合のみ。
-- そのようなケースでは CheckIfAlreadyPrimary が既にエラーを出しているので余分な TypeError は不要。
type family PrimaryDefault (isIntegerPrimaryKey :: Bool) (defaultFrom :: ColumnDefault) :: ColumnDefault where
    PrimaryDefault 'True 'NoDefault = 'ImplicitAutoIncrement
    PrimaryDefault _     'ExplicitDefault = TypeError ErrorMessageDefaultVsAutoIncrement
    PrimaryDefault 'True 'AutoIncrement = 'AutoIncrement                 -- (1)
    PrimaryDefault 'True 'ImplicitAutoIncrement = 'ImplicitAutoIncrement -- (1)
    PrimaryDefault 'False default_ = default_

type ErrorMessageDefaultVsAutoIncrement =
    'Text "You have a DEFAULT for a INTEGER PRIMARY KEY which have implicit AUTOINCREMENT"

-- * UNIQUE
--
-- https://sqlite.org/lang_createtable.html
-- NULL カラムでも UNIQUE制約は付けられる。
-- PRIMARY KEY は暗黙的に UNIQUE になるので別途指定は不要。
-- NULLbility と DEFAULT には関係せず。
--
-- 現状は型レベルで見ていないので,単一カラムで複数回付けられちゃう

unique
    :: Column _name columnType _sqlType _nullable default_ isPrimary
    -> Column _name columnType _sqlType _nullable default_ isPrimary
unique = unsafeAddColumnConstraint CCUnique
