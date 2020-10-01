module Database.Nelda.TabAttr where

import Database.Nelda.Types (AnyColName)

data TabAttr
    = PrimaryKey [AnyColName]
