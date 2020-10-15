{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
module Database.Nelda.Backend.Types where

import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (for_)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text as Text (pack, toLower, take)
import Data.Text.Encoding
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.Typeable (Typeable)
import Data.UUID.Types (toByteString)
import Database.PostgreSQL.LibPQ hiding (user, pass, db, host)
import PostgreSQL.Binary.Encoding as Enc
import PostgreSQL.Binary.Decoding as Dec
import qualified Database.Selda.PostgreSQL.Oid as Oid
import qualified Data.ByteString as BS

-- import qualified Data.ByteString.Char8 as BS (pack, unpack)

{-
sqlite の場合, SqlParam も SqlValue も同じ SQLData
-}

-- | SQL backend にリテラルを渡す際の型
type SqlParam = Maybe (Oid, BS.ByteString, Format)

-- | NULL literal
-- | Needs for implementation of Lit a -> Literal
nullSqlParam :: SqlParam
nullSqlParam = Nothing

-- | SQL backend のクエリの結果絵られるデータの型
type SqlValue = Maybe (Oid, BS.ByteString)

-- | 結果が NULL かどうかの判定。
-- instance SqlType (Maybe a) の実装で必要
isSqlValueNull :: SqlValue -> Bool
isSqlValueNull Nothing = True
isSqlValueNull _       = False

-- | Debug用途
inspectResult :: SqlValue -> Text
inspectResult _ = "TODO"
