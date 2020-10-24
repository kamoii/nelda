module Database.Nelda.SQL.Row where

import Database.Nelda.SQL.Types (UntypedCol)

-- | A database row. A row is a collection of one or more columns.
newtype Row s a = Many [UntypedCol]
