module Database.Nelda.TableAttr where

import Database.Nelda.Types (AnyColumnName)

-- ?? SqlTableType モジュールに入れたほうがいいかな？

data TableAttr
    = PrimaryKey [AnyColumnName]
