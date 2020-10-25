module Database.Nelda.SQL.Row where

import Database.Nelda.SQL.Types (UntypedCol(Untyped))
import Database.Nelda.SqlType (SqlType)
import Data.Data (Proxy)
import Database.Nelda.SQL.Col (Col(One))
import Unsafe.Coerce (unsafeCoerce)

-- | A database row. A row is a collection of one or more columns.
newtype Row s a = Many [UntypedCol]

