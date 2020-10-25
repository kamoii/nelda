module Database.Nelda.SqlOrd where

-- Database.Nelda.SQL.Operation の比較演算子が適用可能な SqlType

import Database.Nelda.SqlType (SqlType)
import Data.Text (Text)

class SqlType a => SqlOrd a

instance SqlOrd Text
instance SqlOrd Int
instance SqlOrd Double
instance SqlOrd a => SqlOrd (Maybe a)
