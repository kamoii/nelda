{-# LANGUAGE EmptyDataDeriving, OverloadedStrings #-}
module Database.Selda.Backend.PPConfig where

import Database.Nelda.SqlType
import Data.Text (Text)
import qualified Data.Text as T

{-
元々 Database.Selda.Table.Type にあったものを抽出。
DB 独自のものがなければ selda-types という selda and backend-impl から参照できるパッケージにする？かな
(コレ recursive component で解決する問題なのかな？)
つまり selda の signature を backend-impl が initiate しつつ, backend-impl から selda に依存。
まあ Recursive Component は今のところサポートされていないようだ..
-}

-- | Strongly or weakly auto-incrementing primary key?
data AutoIncType = Weak | Strong
    deriving (Show, Eq, Ord)

-- | Column attributes such as nullability, auto increment, etc.
--   When adding elements, make sure that they are added in the order
--   required by SQL syntax, as this list is only sorted before being
--   pretty-printed.
data ColAttr
  = Primary
  | AutoPrimary AutoIncType
  | Required
  | Optional
  | Unique
  | Indexed (Maybe IndexMethod)
  deriving (Show, Eq, Ord)

-- | Method to use for indexing with 'indexedUsing'.
--   Index methods are ignored by the SQLite backend, as SQLite doesn't support
--   different index methods.
-- SQLite は index method をサポートしないので Void とするべき。
-- PostgreSQL は BTreeIndex | HashIndex があるはず。
data IndexMethod
    deriving (Show, Eq, Ord)

{-
PPConfig の内容。
基本 connection に限らず決まるため単なるバックエンドの関数にすべし
-}

-- | The SQL type name of the given type.
--
--   This function should be used everywhere a type is needed to be printed but in primary
--   keys position. This is due to the fact that some backends might have a special
--   representation of primary keys (using sequences are such). If you have such a need,
--   please use the 'ppTypePK' record instead.
ppType :: SqlTypeRep -> Text
ppType TInteger = "INTEGER"
ppType TFloat = "DOUBLE"
ppType TText = "TEXT"
ppType TBlob = "BLOB"
ppType TRowID = "INTEGER"
ppType TBoolean = "BOOLEAN"

-- | Hook that allows you to modify 'ppType' output.
ppTypeHook :: SqlTypeRep -> [ColAttr] -> (SqlTypeRep -> Text) -> Text
ppTypeHook ty _ _ = ppType ty

-- | The SQL type name of the given type for primary keys uses.
ppTypePK :: SqlTypeRep -> Text
ppTypePK = ppType

-- | Parameter placeholder for the @n@th parameter.
ppPlaceholder :: Int -> Text
ppPlaceholder = T.cons '$' . T.pack . show

defColAttr :: ColAttr -> Text
defColAttr Primary              = ""
defColAttr (AutoPrimary Strong) = "PRIMARY KEY AUTOINCREMENT"
defColAttr (AutoPrimary Weak)   = "PRIMARY KEY"
defColAttr Required             = "NOT NULL"
defColAttr Optional             = "NULL"
defColAttr Unique               = "UNIQUE"
defColAttr (Indexed _)          = ""

-- | List of column attributes.
ppColAttrs :: [ColAttr] -> Text
ppColAttrs = T.unwords . map defColAttr

-- | Hook that allows you to modify 'ppColAttrs' output.
ppColAttrsHook :: SqlTypeRep -> [ColAttr] -> ([ColAttr] -> Text) -> Text
ppColAttrsHook = \_ ats _ -> T.unwords $ map defColAttr ats

-- | The value used for the next value for an auto-incrementing column.
--   For instance, @DEFAULT@ for PostgreSQL, and @NULL@ for SQLite.
ppAutoIncInsert :: Text
ppAutoIncInsert = "NULL"

-- | Insert queries may have at most this many parameters; if an insertion
--   has more parameters than this, it will be chunked.
--
--   Note that only insertions of multiple rows are chunked. If your table
--   has more than this many columns, you should really rethink
--   your database design.
ppMaxInsertParams :: Maybe Int
ppMaxInsertParams = Just 999

-- | @CREATE INDEX@ suffix to indicate that the index should use the given
--   index method.
ppIndexMethodHook :: IndexMethod -> Text
ppIndexMethodHook = const ""
