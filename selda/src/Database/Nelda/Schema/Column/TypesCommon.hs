{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Schema.Column.TypesCommon where

import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind
import Data.Text (pack, Text)
import Data.Proxy (Proxy(..))
import GHC.Base (Type)
import GHC.TypeLits (symbolVal, KnownSymbol)
import GHC.Base (Symbol)
import GHC.OverloadedLabels (IsLabel(..))

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
