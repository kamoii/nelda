{-# LANGUAGE GADTs, BangPatterns, OverloadedStrings, CPP #-}
-- | Encoding/decoding for PostgreSQL.
module Database.Selda.PostgreSQL.Encoding
  ( toSqlValue, fromSqlValue, fromSqlType, readInt, readBool
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (utc, localToUTCTimeOfDay)
import Database.PostgreSQL.LibPQ (Oid (..), Format (Binary))
import Database.Selda.PostgreSQL.Types
import PostgreSQL.Binary.Encoding as Enc
import PostgreSQL.Binary.Decoding as Dec
import qualified Data.UUID.Types as UUID (toByteString)
import Data.Int (Int16, Int32, Int64)

bytes :: Enc.Encoding -> BS.ByteString
bytes = Enc.encodingBytes

-- | Convert a parameter into an postgres parameter triple.
fromSqlValue :: Lit a -> Maybe (Oid, BS.ByteString, Format)
fromSqlValue = undefined
-- fromSqlValue (LBool b)     = Just (boolType, bytes $ Enc.bool b, Binary)
-- fromSqlValue (LInt n)      = Just ( intType
--                                   , bytes $ Enc.int8_int64 $ fromIntegral n
--                                   , Binary)
-- fromSqlValue (LDouble f)   = Just (doubleType, bytes $ Enc.float8 f, Binary)
-- fromSqlValue (LText s)     = Just (textType, bytes $ Enc.text_strict s, Binary)
-- fromSqlValue (LDateTime t) = Just ( timestampType
--                                   , bytes $ Enc.timestamptz_int t
--                                   , Binary)
-- fromSqlValue (LTime t)     = Just (timeType, bytes $ Enc.timetz_int (t, utc), Binary)
-- fromSqlValue (LDate d)     = Just (dateType, bytes $ Enc.date d, Binary)
-- fromSqlValue (LUUID x)     = Just (uuidType, bytes $ Enc.uuid x, Binary)
-- fromSqlValue (LBlob b)     = Just (blobType, bytes $ Enc.bytea_strict b, Binary)
-- fromSqlValue (LNull)       = Nothing
-- fromSqlValue (LJust x)     = fromSqlValue x
-- fromSqlValue (LCustom TJSON (LBlob b)) = Just ( jsonbType
--                                               , bytes $ Enc.jsonb_bytes b
--                                               , Binary)
-- fromSqlValue (LCustom _ l) = fromSqlValue l

-- | Get the corresponding OID for an SQL type representation.
fromSqlType :: SqlTypeRep -> Oid
fromSqlType = undefined
-- fromSqlType TBool     = boolType
-- fromSqlType TInt      = intType
-- fromSqlType TFloat    = doubleType
-- fromSqlType TText     = textType
-- fromSqlType TDateTime = timestampType
-- fromSqlType TDate     = dateType
-- fromSqlType TTime     = timeType
-- fromSqlType TBlob     = blobType
-- fromSqlType TRowID    = intType
-- fromSqlType TUUID     = uuidType
-- fromSqlType TJSON     = jsonbType

-- | Convert the given postgres return value and type to an @SqlValue@.
toSqlValue :: Oid -> BS.ByteString -> SqlValue
toSqlValue = undefined
-- toSqlValue t val
--   | t == boolType      = SqlBool    $ parse Dec.bool val
--   | t == intType       = SqlInt     $ fromIntegral $ parse (Dec.int :: Value Int64) val
--   | t == int32Type     = SqlInt     $ fromIntegral $ parse (Dec.int :: Value Int32) val
--   | t == int16Type     = SqlInt     $ fromIntegral $ parse (Dec.int :: Value Int16) val
--   | t == doubleType    = SqlFloat   $ parse Dec.float8 val
--   | t == blobType      = SqlBlob    $ parse Dec.bytea_strict val
--   | t == uuidType      = SqlBlob    $ uuid2bs $ parse Dec.uuid val
--   | t == timestampType = SqlUTCTime $ parse parseTimestamp val
--   | t == timeType      = SqlTime    $ toTime $ parse parseTime val
--   | t == dateType      = SqlDate    $ parse Dec.date val
--   | t == jsonbType     = SqlBlob    $ parse (Dec.jsonb_bytes pure) val
--   | t `elem` textish   = SqlString  $ parse Dec.text_strict val
--   | otherwise          = error $ "BUG: result with unknown type oid: " ++ show t
--   where
--     parseTimestamp = Dec.timestamptz_int <|> Dec.timestamptz_float
--     parseTime = Dec.timetz_int <|> Dec.timetz_float
--     toTime (tod, tz) = snd $ localToUTCTimeOfDay tz tod
--     uuid2bs = LBS.toStrict . UUID.toByteString
--     textish = [textType, nameType, varcharType]

parse :: Value a -> BS.ByteString -> a
parse p x =
  case valueParser p x of
    Right x' -> x'
    Left _   -> error "unable to decode value"

-- | Read an Int from a binary encoded pgint8.
readInt :: BS.ByteString -> Int
readInt = fromIntegral . parse (Dec.int :: Value Int64)

readBool :: T.Text -> Bool
readBool = go . T.map toLower
  where
    go "f"     = False
    go "0"     = False
    go "false" = False
    go "n"     = False
    go "no"    = False
    go "off"   = False
    go _       = True
