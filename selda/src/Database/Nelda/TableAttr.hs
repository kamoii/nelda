module Database.Nelda.TableAttr where

import Database.Nelda.Types (AnyColumnName)

data TableAttr
    = PrimaryKey [AnyColumnName]
