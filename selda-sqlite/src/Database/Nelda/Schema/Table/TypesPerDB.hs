module Database.Nelda.Schema.Table.TypesPerDB where

import Database.Nelda.Schema.Column.Types (ColumnName)
import Data.Text (Text)

newtype IndexName = IndexName Text
    deriving Show

data Index = Index
    { indexIsUnique :: Bool
    , indexName :: IndexName
    , indexColumns :: [ColumnName]
    } deriving Show
