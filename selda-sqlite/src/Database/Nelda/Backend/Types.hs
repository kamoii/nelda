{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
module Database.Nelda.Backend.Types where

import Data.Text (Text)
import Database.SQLite3
import qualified Database.SQLite3 as SQLite3
import Control.Exception (Exception)
import Data.Typeable (Typeable)

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

-- | 確立された接続及びメタ情報
type Connection = Database

-- | Prepared Statement Type
type Statement = SQLite3.Statement

-- | Backend が例外
data BackendError
    = DbError String     -- ^ Unable to open or connect to database.
    | SqlError String    -- ^ An error occurred while executing query.
    | UnsafeError String -- ^ An error occurred due to improper use of an unsafe
                         --   function.
    deriving (Show, Eq, Typeable)

instance Exception BackendError
