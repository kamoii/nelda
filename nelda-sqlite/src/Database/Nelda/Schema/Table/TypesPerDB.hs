module Database.Nelda.Schema.Table.TypesPerDB where

import Database.Nelda.Schema.Column.Types (ColumnName)
import Data.Text (pack, Text)
import Data.String (IsString(..))
import Database.Nelda.Schema.Table.TypesCommon (TableName)

newtype IndexName = IndexName Text
    deriving Show

instance IsString IndexName where
    fromString = IndexName . pack

data Index = Index
    { indexIsUnique :: Bool
    , indexName :: IndexName
    , indexTableName :: TableName
    , indexColumns :: [ColumnName]
    } deriving Show
