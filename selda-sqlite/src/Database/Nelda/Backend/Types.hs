{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
module Database.Nelda.Backend.Types where

import Data.Typeable (Typeable)
import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic
import Data.Text as Text (pack, toLower, take)
import Data.Text (Text)
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.UUID.Types (toByteString)
import Database.SQLite3
import System.Directory (makeAbsolute)

{-
sqlite の場合, SqlParam も SqlValue も同じ SQLData
-}

-- | SQL backend にリテラルを渡す際の型
type SqlParam = SQLData

-- | NULL literal
-- | Needs for implementation of Lit a -> Literal
nullSqlParam :: SqlParam
nullSqlParam = SQLNull

-- | SQL backend のクエリの結果絵られるデータの型
type SqlValue = SQLData

-- | 結果が NULL かどうかの判定。
-- instance SqlType (Maybe a) の実装で必要
isSqlValueNull :: SqlValue -> Bool
isSqlValueNull SQLNull = True
isSqlValueNull _       = False

-- | Debug用途
inspectResult :: SqlValue -> Text
inspectResult _ = "TODO"
