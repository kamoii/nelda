{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import GHC.Base (Type)
import Data.String (fromString, IsString)

data ColumnName = ColumnName Text
    deriving (Eq, Ord, Show)

instance IsString ColumnName where
    fromString = ColumnName . pack

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
