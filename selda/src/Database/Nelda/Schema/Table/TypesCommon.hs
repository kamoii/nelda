module Database.Nelda.Schema.Table.TypesCommon where

import Data.Text (pack, Text)
import Data.String (fromString, IsString)

data TableName = TableName Text
    deriving (Eq, Ord, Show)

instance IsString TableName where
    fromString = TableName . pack
