{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Nelda.Schema.Table.TypesCommon where

import GHC.TypeLits (symbolVal, KnownSymbol, Symbol)
import Data.Text (pack, Text)
import GHC.OverloadedLabels (IsLabel(..))
import Data.Data (Proxy(Proxy))

data TableName (s :: Symbol) = TableName Text
    deriving (Show)

instance (KnownSymbol s, s ~ s') => IsLabel s (TableName s') where
    fromLabel = TableName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyTableName = forall s. AnyTableName (TableName s)

deriving instance Show AnyTableName
