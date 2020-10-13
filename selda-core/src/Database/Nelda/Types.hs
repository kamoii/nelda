{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Nelda.Types where

import Data.Text (Text)
import Data.String (IsString)

-- * SqlFragment
-- CREATE文も SQLの一部だよ。

newtype SqlFragment = SqlFragment Text
    deriving (Show, Eq, IsString, Semigroup, Monoid)


newtype Sql = Sql Text
    deriving (Show, Eq, IsString, Semigroup, Monoid)
