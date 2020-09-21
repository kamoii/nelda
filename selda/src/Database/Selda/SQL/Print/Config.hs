{-# LANGUAGE OverloadedStrings #-}
module Database.Selda.SQL.Print.Config (PPConfig (..), ppConfig) where
import Data.Text (Text)
import qualified Data.Text as T
import Database.Selda.SqlType
import Database.Selda.Table
import qualified Database.Selda.Backend.Types as BE
import qualified Database.Selda.Backend.PPConfig as BE

-- | Backend-specific configuration for the SQL pretty-printer.
data PPConfig = PPConfig
  { -- | The SQL type name of the given type.
    --
    --   This function should be used everywhere a type is needed to be printed but in primary
    --   keys position. This is due to the fact that some backends might have a special
    --   representation of primary keys (using sequences are such). If you have such a need,
    --   please use the 'ppTypePK' record instead.
    ppType :: SqlTypeRep -> Text

    -- | Hook that allows you to modify 'ppType' output.
  , ppTypeHook :: SqlTypeRep -> [ColAttr] -> (SqlTypeRep -> Text) -> Text

    -- | The SQL type name of the given type for primary keys uses.
  , ppTypePK :: SqlTypeRep -> Text

    -- | Parameter placeholder for the @n@th parameter.
  , ppPlaceholder :: Int -> Text

    -- | List of column attributes.
  , ppColAttrs :: [ColAttr] -> Text

    -- | Hook that allows you to modify 'ppColAttrs' output.
  , ppColAttrsHook :: SqlTypeRep -> [ColAttr] -> ([ColAttr] -> Text) -> Text

    -- | The value used for the next value for an auto-incrementing column.
    --   For instance, @DEFAULT@ for PostgreSQL, and @NULL@ for SQLite.
  , ppAutoIncInsert :: Text

    -- | Insert queries may have at most this many parameters; if an insertion
    --   has more parameters than this, it will be chunked.
    --
    --   Note that only insertions of multiple rows are chunked. If your table
    --   has more than this many columns, you should really rethink
    --   your database design.
  , ppMaxInsertParams :: Maybe Int

    -- | @CREATE INDEX@ suffix to indicate that the index should use the given
    --   index method.
  , ppIndexMethodHook :: IndexMethod -> Text
  }

-- | Default settings for pretty-printing.
--   Geared towards SQLite.
--
--   The default definition of 'ppTypePK' is 'defType, so that you don’t have to do anything
--   special if you don’t use special types for primary keys.
ppConfig :: PPConfig
ppConfig = PPConfig
    { ppType = BE.ppType
    , ppTypeHook = BE.ppTypeHook
    , ppTypePK = BE.ppTypePK
    , ppPlaceholder = BE.ppPlaceholder
    , ppColAttrs = BE.ppColAttrs
    , ppColAttrsHook = BE.ppColAttrsHook
    , ppAutoIncInsert = BE.ppAutoIncInsert
    , ppMaxInsertParams = BE.ppMaxInsertParams
    , ppIndexMethodHook = BE.ppIndexMethodHook
    }

-- -- | Default compilation for SQL types.
-- --   By default, anything we don't know is just a blob.
-- defType :: SqlTypeRep -> Text
-- defType = BE.sqlTypeDef

-- -- | Default compilation for a column attribute.
-- defColAttr :: ColAttr -> Text
-- defColAttr Primary              = ""
-- defColAttr (AutoPrimary Strong) = "PRIMARY KEY AUTOINCREMENT"
-- defColAttr (AutoPrimary Weak)   = "PRIMARY KEY"
-- defColAttr Required             = "NOT NULL"
-- defColAttr Optional             = "NULL"
-- defColAttr Unique               = "UNIQUE"
-- defColAttr (Indexed _)          = ""
