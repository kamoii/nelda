{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
module Database.Selda.PostgreSQL.Types where

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

-- | Representation of an SQL type.
-- 通常 TText, TInt, TDateTime のような形式
-- TRowID は SQL backend の型ではなく, selda の都合上導入されていた論理型なので不要
-- `BOOLEAN' などの別名はサポートするべきか？
data SqlTypeRep
    = TRowID   -- DEPRECATE予定
    | TInt
    | TBool
    | TDouble
    | TText
    | TBlob
    | TUUID
    deriving (Show, Eq, Ord)
-- fromSqlType TDateTime = timestampType
-- fromSqlType TDate     = dateType
-- fromSqlType TTime     = timeType

rowIDSqlType :: SqlTypeRep
rowIDSqlType = TRowID

isCompatibleWith :: SqlTypeRep -> SqlTypeRep -> Bool
isCompatibleWith TRowID TInt = True
isCompatibleWith TInt TRowID = True
isCompatibleWith a b         = a == b

-- | Any datatype representable in SQL.
-- 実際にサポートされている型のみ。
-- NULL や 抽象化された ID は SqlType 型クラス('なし)で考慮する。
-- というかここでは instance 定義はできあにので,全ての backend で Maybe a や ID a の instance 実装が必要になる。
-- あと SqlType, SqlType' を分けないと orphan instance の問題が。
-- まあ関連ライブラリのみに閉じているからいいかもだけど...
--
-- TDOO: SqlType, SqlType' を分けているのが吉と出るか...
-- TODO: Typeable 制約は SqlType のために付けているだけで後から外せるかも
class Typeable a => SqlType' a where
    -- | Create a literal of this type.
    toSqlParam :: a -> SqlParam
    -- | The SQL representation for this type.
    sqlTypeRep :: SqlTypeRep
    -- | Convert an SqlValue into this type.
    fromSqlValue :: SqlValue -> a
    -- | Pring for insperct purpose
    inspectPrint :: a -> Text
    -- | Default value when using 'def' at this type.
    -- TODO: DEPRECATE。DEFAULTカラムの insert 時に使っているがこれは本来ライブラリが決めるべき値ではない。
    defaultValue :: a

bytes :: Enc.Encoding -> BS.ByteString
bytes = Enc.encodingBytes

parse :: Value a -> BS.ByteString -> a
parse p x =
  case valueParser p x of
    Right x' -> x'
    Left _   -> error "unable to decode value"

-- これは Connection.hs の実装時の SqlString Pattern Synonim に使っている
extractStringLike :: SqlValue -> Maybe Text
extractStringLike (Just (oid, val)) | isStringLike oid = Just (parse Dec.text_strict val)
  where isStringLike oid = oid `elem` [Oid.textType, Oid.nameType, Oid.varcharType]
extractStringLike _ = Nothing

extractBool :: SqlValue -> Maybe Bool
extractBool (Just (oid, val)) | oid == Oid.boolType = Just (parse Dec.bool val)
extractBool _ = Nothing

instance SqlType' Int where
    toSqlParam i = Just ( Oid.intType, bytes $ Enc.int8_int64 $ fromIntegral i, Binary)
    sqlTypeRep = TInt
    fromSqlValue (Just (oid, val)) | oid == Oid.intType = fromIntegral $ parse (Dec.int :: Value Int64) val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    inspectPrint = pack . show
    defaultValue = 0

instance SqlType' Text where
    toSqlParam t = Just ( Oid.textType, bytes $ Enc.text_strict t, Binary)
    sqlTypeRep = TText
    -- TODO: selda の元々の実装では oid は [textType, nameType, varcharType] の内いずれでも許容していた。
    -- TEXT型なのに oid == textType 以外返りえるのか？
    fromSqlValue (Just (oid, val)) | oid == Oid.textType = parse Dec.text_strict val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    inspectPrint = id
    defaultValue = ""

instance SqlType' Double where
    toSqlParam d = Just (Oid.doubleType, bytes $ Enc.float8 d, Binary)
    sqlTypeRep = TDouble
    fromSqlValue (Just (oid, val)) | oid == Oid.doubleType = parse Dec.float8 val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    inspectPrint = pack . show
    defaultValue = 0.0

instance SqlType' Bool where
    toSqlParam b = Just (Oid.boolType, bytes $ Enc.bool b, Binary)
    sqlTypeRep = TBool
    fromSqlValue (Just (oid, val)) | oid == Oid.boolType = parse Dec.bool val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    inspectPrint = pack . show
    defaultValue = False   -- TODO: やっぱ defaultValue って決まらんわ

-- | Any column type that can be used with the 'min_' and 'max_' functions.
-- | Int
class SqlType' a => SqlOrdable a
