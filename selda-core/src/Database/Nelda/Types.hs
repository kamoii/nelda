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

-- * ColName

data ColName (s :: Symbol) = ColName Text

instance (KnownSymbol s, s ~ s') => IsLabel s (ColName s') where
    fromLabel = ColName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyColName = forall s. AnyColName (ColName s)

-- * TabName

data TabName (s :: Symbol) = TabName Text

instance (KnownSymbol s, s ~ s') => IsLabel s (TabName s') where
    fromLabel = TabName $ pack $ symbolVal (Proxy :: Proxy s)

data AnyTabName = forall s. AnyTabName (TabName s)
