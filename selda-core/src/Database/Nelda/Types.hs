{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Types where

import GHC.TypeLits as TL (symbolVal, KnownSymbol, Symbol)
import Data.Text (pack, Text)
import GHC.OverloadedLabels (IsLabel(..))
import Data.Data (Proxy(Proxy))
import Data.String (IsString)

-- * SqlFragment
-- CREATE文も SQLの一部だよ。

newtype SqlFragment = SqlFragment Text
    deriving (Show, Eq, IsString)

-- * ColumnName

data ColumnName (s :: Symbol) = ColumnName Text

instance (KnownSymbol s, s ~ s') => IsLabel s (ColumnName s') where
    fromLabel = ColumnName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyColumnName = forall s. AnyColumnName (ColumnName s)

-- * TableName

data TableName (s :: Symbol) = TableName Text

instance (KnownSymbol s, s ~ s') => IsLabel s (TableName s') where
    fromLabel = TableName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyTableName = forall s. AnyTableName (TableName s)
