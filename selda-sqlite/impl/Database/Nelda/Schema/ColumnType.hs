{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Nelda.Schema.ColumnType where

import Database.Nelda.Schema.Types (ColumnName)
import Database.Nelda.Schema.SqlColumnType
import Database.Nelda.Types (SqlFragment(..))
import Database.Nelda.SqlType (SqlType)
import Data.Kind (Type)
import Data.Data (Proxy)
import Data.Text (Text)

-- _type の使いかたは安全ではない(unsafe preifx 付けるか？)
-- ライブラリを実装する上での注意点だが, SqlColumnTypeRep は ct に対応するものでないといけない。
-- TODO: これ修正するか？
-- いやライブラリだけの安全性を無理して得る必要はないかな。
-- 実装するなら SqlColumnType 型クラスに type class を追加する必要が出てくる
_unsafeMkType :: SqlColumnType ct => SqlColumnTypeRep -> ColumnType ct (ToSqlType ct)
_unsafeMkType rep = ColumnType rep

int :: ColumnType 'TInt Int
int = _unsafeMkType RInt

text :: ColumnType 'TText Text
text = _unsafeMkType RText

