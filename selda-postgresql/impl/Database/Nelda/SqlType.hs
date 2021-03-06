{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.SqlType
    ( module Database.Nelda.Backend.Types
    , module Database.Nelda.SqlType
    ) where

import Database.Nelda.Backend.Types
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


class (Typeable a, Show a) => SqlType a where
    type OriginSqlType a
    -- | The SQL representation for this type.
    sqlTypeRep :: SqlTypeRep
    -- | Create a literal of this type.
    toSqlParam :: a -> SqlParam
    -- | Convert an SqlValue into this type.
    fromSqlValue :: SqlValue -> a
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

instance SqlType a => SqlType (Maybe a) where
    type OriginSqlType (Maybe a) = Maybe (OriginSqlType a)
    sqlTypeRep = sqlTypeRep @a
    toSqlParam Nothing = nullSqlParam
    toSqlParam (Just a) = toSqlParam a
    fromSqlValue v
        | isSqlValueNull v = Nothing
        | otherwise = Just $ fromSqlValue v
    defaultValue = Nothing

instance SqlType Int where
    type OriginSqlType Int = Int
    sqlTypeRep = TInt
    toSqlParam i = Just ( Oid.intType, bytes $ Enc.int8_int64 $ fromIntegral i, Binary)
    fromSqlValue (Just (oid, val)) | oid == Oid.intType = fromIntegral $ parse (Dec.int :: Value Int64) val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    defaultValue = 0

instance SqlType Text where
    type OriginSqlType Text = Text
    sqlTypeRep = TText
    toSqlParam t = Just ( Oid.textType, bytes $ Enc.text_strict t, Binary)
    -- TODO: selda の元々の実装では oid は [textType, nameType, varcharType] の内いずれでも許容していた。
    -- TEXT型なのに oid == textType 以外返りえるのか？
    fromSqlValue (Just (oid, val)) | oid == Oid.textType = parse Dec.text_strict val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    defaultValue = ""

instance SqlType Double where
    type OriginSqlType Double = Double
    sqlTypeRep = TDouble
    toSqlParam d = Just (Oid.doubleType, bytes $ Enc.float8 d, Binary)
    fromSqlValue (Just (oid, val)) | oid == Oid.doubleType = parse Dec.float8 val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    defaultValue = 0.0

instance SqlType Bool where
    type OriginSqlType Bool = Bool
    sqlTypeRep = TBool
    toSqlParam b = Just (Oid.boolType, bytes $ Enc.bool b, Binary)
    fromSqlValue (Just (oid, val)) | oid == Oid.boolType = parse Dec.bool val
    fromSqlValue _ = error "Unexpected fromSqlValue"
    defaultValue = False   -- TODO: やっぱ defaultValue って決まらんわ

-- | Any column type that can be used with the 'min_' and 'max_' functions.
-- | Int
class SqlType a => SqlOrdable a
