{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Schema.Column.Types where

import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind
import Data.Text (pack, Text)
import Data.Proxy (Proxy(..))
import GHC.Base (Type)
import GHC.TypeLits (symbolVal, KnownSymbol)
import GHC.Base (Symbol)
import GHC.OverloadedLabels (IsLabel(..))

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

-- * ColumnName

data ColumnName (s :: Symbol) = ColumnName Text
    deriving (Show)

instance (KnownSymbol s, s ~ s') => IsLabel s (ColumnName s') where
    fromLabel = ColumnName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyColumnName = forall s. AnyColumnName (ColumnName s)

deriving instance Show AnyColumnName

data ColumnType (ct :: SqlColumnTypeKind) (st :: Type) = ColumnType SqlColumnTypeRep
    deriving (Show)

data ColumnNull
    = Nullable
    | NotNull
    | ImplicitNotNull
    deriving (Eq, Show)

data ColumnDefault
    = NoDefault
    | AutoIncrement
    | ExplicitDefault
    | ImplicitAutoIncrement
    deriving (Eq, Show)
