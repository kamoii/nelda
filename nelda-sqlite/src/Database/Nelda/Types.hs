{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Nelda.Types where

import Data.Text (Text)
import Data.String (IsString)

newtype Sql = Sql Text
    deriving (Show, Eq, IsString, Semigroup, Monoid)
