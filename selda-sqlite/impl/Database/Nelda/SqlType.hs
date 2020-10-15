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

-- | Representation of an SQL type.
-- 通常 TText, TInt, TDateTime のような形式
-- TRowID は SQL backend の型ではなく, selda の都合上導入されていた論理型なので不要
-- `BOOLEAN' などの別名はサポートするべきか？
data SqlTypeRep
    = TInteger
    | TFloat
    | TText
    | TBlob
    | TRowID   -- DEPRECATE予定
    | TBoolean
    deriving (Show, Eq, Ord)

rowIDSqlType :: SqlTypeRep
rowIDSqlType = TRowID

isCompatibleWith :: SqlTypeRep -> SqlTypeRep -> Bool
isCompatibleWith TRowID TInteger = True
isCompatibleWith TInteger TRowID = True
isCompatibleWith a b             = a == b

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
    sqlTypeRep = TInteger
    toSqlParam i = SQLInteger $ fromIntegral i -- TODO: いいのか？
    fromSqlValue (SQLInteger i) = fromIntegral i  -- TODO: いいのか？
    defaultValue = 0

instance SqlType Text where
    type OriginSqlType Text = Text
    sqlTypeRep = TText
    toSqlParam t = SQLText t
    fromSqlValue (SQLText t) = t
    defaultValue = ""

instance SqlType Double where
    type OriginSqlType Double = Double
    sqlTypeRep = TFloat
    toSqlParam d = SQLFloat d
    fromSqlValue (SQLFloat d) = d
    defaultValue = 0.0

instance SqlType Bool where
    type OriginSqlType Bool = Bool
    sqlTypeRep = TBoolean
    toSqlParam b = SQLInteger $ if b then 1 else 0
    fromSqlValue (SQLInteger i) = not (i==0)
    defaultValue = False   -- TODO: やっぱ defaultValue って決まらんわ

-- | Any column type that can be used with the 'min_' and 'max_' functions.
-- | Int
class SqlType a => SqlOrdable a
