{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, DefaultSignatures, DeriveGeneric, TypeApplications #-}
-- | Types representable as columns in Selda's subset of SQL.
module Database.Selda.SqlType
  ( SqlType
  , Database.Selda.SqlType.defaultValue
  , fromSql
  , sqlType
  , mkLit
  , Lit (..), UUID, UUID', RowID, ID, SqlValue (..), SqlTypeRep (..)
  , invalidRowId, isInvalidRowId, toRowId, fromRowId
  , fromId, toId, invalidId, isInvalidId, untyped
  , litType, litToSqlParam
  , sqlDateTimeFormat, sqlDateFormat, sqlTimeFormat
  , typedUuid, untypedUuid
  ) where
import Control.Applicative ((<|>))
import Control.Exception (Exception (..), throw)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Typeable
import Data.UUID.Types (UUID, toString, fromByteString, nil)
import GHC.Generics (Generic)

import qualified Database.Selda.Backend.Connection as BE

import Database.Nelda.SqlType (SqlType(..), SqlTypeRep(..), SqlValue, SqlParam, isSqlValueNull, nullSqlParam, rowIDSqlType)

-- | Format string used to represent date and time when
--   representing timestamps as text.
--   If at all possible, use 'SqlUTCTime' instead.
sqlDateTimeFormat :: String
sqlDateTimeFormat = "%F %H:%M:%S%Q%z"

-- | Format string used to represent date when
--   representing dates as text.
--   If at all possible, use 'SqlDate' instead.
sqlDateFormat :: String
sqlDateFormat = "%F"

-- | Format string used to represent time of day when
--   representing time as text.
--   If at all possible, use 'SqlTime' instead.
sqlTimeFormat :: String
sqlTimeFormat = "%H:%M:%S%Q%z"

-- * SqlType

mkLit :: SqlType a => a -> Lit a
mkLit = LLiteral

sqlType :: forall a. SqlType a => Proxy a -> SqlTypeRep
sqlType _ = sqlTypeRep @a

fromSql :: SqlType a => SqlValue -> a
fromSql sv = fromSqlValue sv

defaultValue :: forall a. SqlType a => Lit a
defaultValue = LLiteral $ Database.Nelda.SqlType.defaultValue @a

-- ??? たぶん Col s Ordering をどっかで使うためにかな...
-- とりあえず hacky ...
instance SqlType Ordering

-- * Lit a

-- | An SQL literal.
-- TODO: Backpack Backend が完成したら LCustom は不要になるかも
-- -> いや LCustom は SQL構築が使われているな。。
-- Lit a は SQL構築のための GADTs という意識が必要
-- TODO: LCustom 使っている箇所を確認してコメントに明記
--
--  * Database.Selda.Prepared での LCustom 利用
--    PlaceHolder にあたる Col s a を作成するために利用。
--    Lit a では throw するという hacky な実装になっている。
--    本来であれば LPlaceHolder constructor を用意するのが正しいかな？
--    ただし Prepared以外では利用されることはなく SqlParam への変換はできないので,
--    いずれにせよ error は一つ必要になる。
--
-- TODO: LLiteral a に Maybe Int みたいなが許容されるけど, LJust と LNull ってまだ要るのか？
data Lit a where
  LLiteral :: SqlType a => a -> Lit a
  LJust    :: SqlType a => !(Lit a) -> Lit (Maybe a)
  LNull    :: SqlType a => Lit (Maybe a)
  LCustom  :: SqlTypeRep -> Lit a -> Lit b

instance Show (Lit a) where
  show (LLiteral a)  = show a
  show (LJust x)     = "Just " ++ show x
  show (LNull)       = "Nothing"
  show (LCustom _ l) = show l

-- | The SQL type representation for the given literal.
litType :: forall a. Lit a -> SqlTypeRep
litType (LLiteral _)  = sqlTypeRep @a
litType (LJust x)     = litType x
litType (x@LNull)     = sqlType (proxyFor x)
  where
    proxyFor :: forall b. Lit (Maybe b) -> Proxy b
    proxyFor _ = Proxy
litType (LCustom t _) = t

litToSqlParam :: Lit a -> SqlParam
litToSqlParam (LLiteral a) = toSqlParam a
litToSqlParam (LJust a)    = litToSqlParam a
litToSqlParam LNull        = nullSqlParam

-- * RowID, ID

-- | A row identifier for some table.
--   This is the type of auto-incrementing primary keys.
newtype RowID = RowID Int
  deriving (Eq, Ord, Typeable, Generic)
instance Show RowID where
  show (RowID n) = show n

-- | A row identifier which is guaranteed to not match any row in any table.
invalidRowId :: RowID
invalidRowId = RowID (-1)

-- | Is the given row identifier invalid? I.e. is it guaranteed to not match any
--   row in any table?
isInvalidRowId :: RowID -> Bool
isInvalidRowId (RowID n) = n < 0

-- | Create a row identifier from an integer.
--   Use with caution, preferably only when reading user input.
toRowId :: Int -> RowID
toRowId = RowID

-- | Inspect a row identifier.
fromRowId :: RowID -> Int
fromRowId (RowID n) = n

-- | A typed row identifier.
--   Generic tables should use this instead of 'RowID'.
--   Use 'untyped' to erase the type of a row identifier, and @cast@ from the
--   "Database.Selda.Unsafe" module if you for some reason need to add a type
--   to a row identifier.
newtype ID a = ID {untyped :: RowID}
  deriving (Eq, Ord, Typeable, Generic)
instance Show (ID a) where
  show = show . untyped

-- | An UUID identifying a database row.
newtype UUID' a = UUID { untypedUuid :: UUID }
  deriving (Eq, Ord, Typeable, Generic)
instance Show (UUID' a) where
  show = show . untypedUuid

-- | Convert an untyped UUID to a typed one.
--   Use sparingly, preferably only during deserialization.
typedUuid :: UUID -> UUID' a
typedUuid = UUID

-- | Create a typed row identifier from an integer.
--   Use with caution, preferably only when reading user input.
toId :: Int -> ID a
toId = ID . toRowId

-- | Create a typed row identifier from an integer.
--   Use with caution, preferably only when reading user input.
fromId :: ID a -> Int
fromId (ID i) = fromRowId i

-- | A typed row identifier which is guaranteed to not match any row in any
--   table.
invalidId :: ID a
invalidId = ID invalidRowId

-- | Is the given typed row identifier invalid? I.e. is it guaranteed to not
--   match any row in any table?
isInvalidId :: ID a -> Bool
isInvalidId = isInvalidRowId . untyped

fromSqlError :: String -> a
fromSqlError = throw . FromSqlError

newtype FromSqlError = FromSqlError String
instance Show FromSqlError where
  show (FromSqlError e) = "[SELDA BUG] fromSql: " ++ e
instance Exception FromSqlError

-- TODO: Deprecated
-- DEPRECATE 予定
-- SqlType の型が auto-incremnt 及び primary key の情報を持つべきではないかな..
instance SqlType RowID where
    type OriginSqlType RowID = Int
    sqlTypeRep = rowIDSqlType
    -- | Create a literal of this type.
    -- TODO: これいいのか？
    toSqlParam (RowID n) = toSqlParam n
    -- | Convert an SqlValue into this type.
    fromSqlValue = RowID . fromSqlValue
    -- | Default value when using 'def' at this type.
    -- TODO: DEPRECATE。DEFAULTカラムの insert 時に使っているがこれは本来ライブラリが決めるべき値ではない。
    -- 例えば Bool の DefaultValue って分からんぞ,という感じ
    defaultValue = invalidRowId
    -- mkLit (RowID n) = LCustom BE.rowIDSqlType (mkLit @Int n)
    -- sqlType _ = BE.rowIDSqlType
    -- fromSql sv
    --     | BE.isSqlValueNull sv = error "Unexpeted NULL"
    --     | otherwise = RowID $ BE.fromSqlValue @Int sv
    -- defaultValue = mkLit invalidRowId

-- instance Typeable a => SqlType (ID a) where
--   mkLit (ID n) = LCustom TRowID (mkLit n)
--   sqlType _ = TRowID
--   fromSql = ID . fromSql
--   defaultValue = mkLit (ID invalidRowId)

-- instance SqlType Int where
--   mkLit = LInt
--   sqlType _ = TInt
--   fromSql (SqlInt x) = x
--   fromSql v          = fromSqlError $ "int column with non-int value: " ++ show v
--   defaultValue = LInt 0

-- instance SqlType Double where
--   mkLit = LDouble
--   sqlType _ = TFloat
--   fromSql (SqlFloat x) = x
--   fromSql v            = fromSqlError $ "float column with non-float value: " ++ show v
--   defaultValue = LDouble 0

-- instance SqlType Text where
--   mkLit = LText
--   sqlType _ = TText
--   fromSql (SqlString x) = x
--   fromSql v             = fromSqlError $ "text column with non-text value: " ++ show v
--   defaultValue = LText ""

-- instance SqlType Bool where
--   mkLit = LBool
--   sqlType _ = TBool
--   fromSql (SqlBool x) = x
--   fromSql (SqlInt 0)  = False
--   fromSql (SqlInt _)  = True
--   fromSql v           = fromSqlError $ "bool column with non-bool value: " ++ show v
--   defaultValue = LBool False

-- instance SqlType UTCTime where
--   mkLit = LDateTime
--   sqlType _ = TDateTime
--   fromSql (SqlUTCTime t) = t
--   fromSql (SqlString s) =
--     case withWeirdTimeZone sqlDateTimeFormat (unpack s) of
--       Just t -> t
--       _      -> fromSqlError $ "bad datetime string: " ++ unpack s
--   fromSql v = fromSqlError $ "datetime column with non-datetime value: " ++ show v
--   defaultValue = LDateTime $ UTCTime (ModifiedJulianDay 40587) 0

-- instance SqlType Day where
--   mkLit = LDate
--   sqlType _ = TDate
--   fromSql (SqlDate d) = d
--   fromSql (SqlString s) =
--     case parseTimeM True defaultTimeLocale sqlDateFormat (unpack s) of
--       Just t -> t
--       _      -> fromSqlError $ "bad date string: " ++ unpack s
--   fromSql v = fromSqlError $ "date column with non-date value: " ++ show v
--   defaultValue = LDate $ ModifiedJulianDay 40587

-- instance SqlType TimeOfDay where
--   mkLit = LTime
--   sqlType _ = TTime
--   fromSql (SqlTime s) = s
--   fromSql (SqlString s) =
--     case withWeirdTimeZone sqlTimeFormat (unpack s) of
--       Just t -> t
--       _      -> fromSqlError $ "bad time string: " ++ unpack s
--   fromSql v = fromSqlError $ "time column with non-time value: " ++ show v
--   defaultValue = LTime $ TimeOfDay 0 0 0

-- -- | Both PostgreSQL and SQLite to weird things with time zones.
-- --   Long term solution is to use proper binary types internally for
-- --   time values, so this is really just an interim solution.
-- withWeirdTimeZone :: ParseTime t => String -> String -> Maybe t
-- withWeirdTimeZone fmt s =
--   parseTimeM True defaultTimeLocale fmt (s++"00")
--   <|> parseTimeM True defaultTimeLocale fmt s
--   <|> parseTimeM True defaultTimeLocale fmt (s++"+0000")

-- instance SqlType ByteString where
--   mkLit = LBlob
--   sqlType _ = TBlob
--   fromSql (SqlBlob x) = x
--   fromSql v           = fromSqlError $ "blob column with non-blob value: " ++ show v
--   defaultValue = LBlob empty

-- instance SqlType BSL.ByteString where
--   mkLit = LCustom TBlob . LBlob . BSL.toStrict
--   sqlType _ = TBlob
--   fromSql (SqlBlob x) = BSL.fromStrict x
--   fromSql v           = fromSqlError $ "blob column with non-blob value: " ++ show v
--   defaultValue = LCustom TBlob (LBlob empty)

-- -- | @defaultValue@ for UUIDs is the all-zero RFC4122 nil UUID.
-- instance SqlType UUID where
--   mkLit = LUUID
--   sqlType _ = TUUID
--   fromSql (SqlBlob x) = fromJust . fromByteString $ BSL.fromStrict x
--   fromSql v           = fromSqlError $ "UUID column with non-blob value: " ++ show v
--   defaultValue = LUUID nil

-- -- | @defaultValue@ for UUIDs is the all-zero RFC4122 nil UUID.
-- instance Typeable a => SqlType (UUID' a) where
--   mkLit = LCustom TUUID . LUUID . untypedUuid
--   sqlType _ = TUUID
--   fromSql = typedUuid . fromSql
--   defaultValue = LCustom TUUID (LUUID nil)
