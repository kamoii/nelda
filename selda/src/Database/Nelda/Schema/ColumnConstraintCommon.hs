{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Database.Nelda.Schema.ColumnConstraintCommon where

import Database.Nelda.Schema.Column.Types
import Database.Nelda.SqlTypeClass
import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | NOT NULL および DEFAULT は共通で。

-- * NOT NULL

type ErrorMessageDuplicatedNotNull =
    'Text "NOT NULL is already explicitly specified."

-- ImplicitNotNull の際は警告でも出したいが,警告が出しづらい上に害はないので。
type family CanSetNotNull (n :: ColumnNull) :: Constraint where
   CanSetNotNull 'Nullable = ()
   CanSetNotNull 'NotNull = TypeError ErrorMessageDuplicatedNotNull
   CanSetNotNull 'ImplicitNotNull = ()

notNull
    :: CanSetNotNull nullable
    => Column _name _columnType _sqlType nullable _default
    -> Column _name _columnType _sqlType 'NotNull _default
notNull c = c { constraintNotNull = True }

-- * DEFAULT

-- TODO: エラーメッセージの改善
-- TODO: defualt_ という名前のほうがいいかな？
-- TODO: 関数呼びだし系の DEAFULT もあるので unsafeDefault も必要かな？
default_
    :: SqlType sqlType
    => sqlType
    -> Column _name _columnType sqlType _nullability 'NoDefault
    -> Column _name _columnType sqlType _nullability 'ExplicitDefault
default_ v c = c { constraintDefault = Just $ toSqlExpression v }
