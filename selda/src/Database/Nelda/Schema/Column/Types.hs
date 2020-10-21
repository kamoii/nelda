{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Schema.Column.Types
    ( module Database.Nelda.Schema.Column.Types
    , module Database.Nelda.Schema.Column.TypesCommon
    , module Database.Nelda.Schema.Column.TypesPerDB
    , module Data.Tagged
    ) where

import Database.Nelda.Schema.Column.TypesCommon
import Database.Nelda.Schema.Column.TypesPerDB
import Data.Tagged
import Data.Text (Text)
import GHC.TypeLits (Symbol)

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

data Column
     (name :: Symbol)
     columnType
     sqlType
     (nullability :: ColumnNull)
     (default_ :: ColumnDefault)
     (isPrimary :: Bool)
    = Column
    { colName :: ColumnName
    , colType :: ColumnType columnType sqlType
    , constraintNotNull :: Bool
    , constraintAutoIncrement :: Bool
    , constraintDefault :: Maybe Text
    , constraints :: [ColumnConstraint]
    }

deriving instance Show (Column name columnType sqlType nullability default_ isPrimary)

data AnyColumn = forall a b c d e f. AnyColumn (Column a b c d e f)

deriving instance Show AnyColumn

data Columns (cols :: [*]) = Columns [AnyColumn]
    deriving (Show)
