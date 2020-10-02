{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Schema.Types where

import GHC.TypeLits as TL (symbolVal, KnownSymbol, Symbol)
import GHC.OverloadedLabels (IsLabel(..))
import Data.Text (pack, Text)
import Data.Data (Proxy(Proxy))

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
