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
-- * TableName
-- * Enum カラム型

class SqlENUM a where
    toENUMText :: a -> Text
    fromENUMText :: Text -> a

newtype ENUM e = ENUM e
    deriving (Eq, Ord, Show)
