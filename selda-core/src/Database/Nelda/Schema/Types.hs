{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Nelda.Schema.Types where

import GHC.TypeLits as TL (symbolVal, KnownSymbol, Symbol)
import GHC.OverloadedLabels (IsLabel(..))
import Data.Text (pack, Text)
import Data.Data (Proxy(Proxy))

-- * ColumnName

data ColumnName (s :: Symbol) = ColumnName Text
    deriving (Show)

instance (KnownSymbol s, s ~ s') => IsLabel s (ColumnName s') where
    fromLabel = ColumnName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyColumnName = forall s. AnyColumnName (ColumnName s)

deriving instance Show AnyColumnName

-- * TableName

data TableName (s :: Symbol) = TableName Text
    deriving (Show)

instance (KnownSymbol s, s ~ s') => IsLabel s (TableName s') where
    fromLabel = TableName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyTableName = forall s. AnyTableName (TableName s)

deriving instance Show AnyTableName

-- * Enum カラム型

class SqlENUM a where
    toENUMText :: a -> Text
    fromENUMText :: Text -> a

newtype ENUM e = ENUM e
    deriving (Eq, Ord, Show)
