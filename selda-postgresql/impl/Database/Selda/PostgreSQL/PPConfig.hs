{-# LANGUAGE EmptyDataDeriving, OverloadedStrings #-}
module Database.Selda.PostgreSQL.PPConfig where

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
--   Index methods are ignored by the PostgreSQL backend, as PostgreSQL doesn't support
--   different index methods.
-- PostgreSQL は index method をサポートしないので Void とするべき。
-- PostgreSQL は BTreeIndex | HashIndex があるはず。
data IndexMethod = BTreeIndex | HashIndex
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
ppType TRowID    = "BIGINT"
ppType TInt      = "INT8"
ppType TDouble   = "FLOAT8"
-- pgType TDateTime = "TIMESTAMP"
ppType TBlob     = "BYTEA"
ppType TUUID     = "UUID"
-- pgType TJSON     = "JSONB"
ppType TText     = "TEXT"
ppType TBool     = "BOOLEAN"


-- For when we use 'autoPrimaryGen' on 'Int' field
isGenericIntPrimaryKey :: SqlTypeRep -> [ColAttr] -> Bool
isGenericIntPrimaryKey ty attrs = ty == TInt && and ((`elem` attrs) <$> bigserialQue)
  where
    bigserialQue :: [ColAttr]
    bigserialQue = [AutoPrimary Strong, Required]

-- | Hook that allows you to modify 'ppType' output.
ppTypeHook :: SqlTypeRep -> [ColAttr] -> (SqlTypeRep -> Text) -> Text
ppTypeHook ty attrs fun
    | isGenericIntPrimaryKey ty attrs = ppTypePK TRowID
    | otherwise                       = pgTypeRenameHook fun ty
  where
    -- pgTypeRenameHook _ TDateTime = "timestamp with time zone"
    -- pgTypeRenameHook _ TTime     = "time with time zone"
    pgTypeRenameHook f ty'        = f ty'

-- | The SQL type name of the given type for primary keys uses.
-- TODO: PostgreSQL には BIGSERIAL という auto-increment機能付き型が存在する。
-- つまり型が auto-incremnt 性を含んでるということ....
ppTypePK :: SqlTypeRep -> Text
ppTypePK TRowID = "BIGSERIAL"
ppTypePK rep    = ppType rep

-- | Parameter placeholder for the @n@th parameter.
ppPlaceholder :: Int -> Text
ppPlaceholder = T.cons '$' . T.pack . show

pgColAttr :: ColAttr -> Text
pgColAttr Primary         = ""
pgColAttr (AutoPrimary _) = "PRIMARY KEY"
pgColAttr Required        = "NOT NULL"
pgColAttr Optional        = "NULL"
pgColAttr Unique          = "UNIQUE"
pgColAttr (Indexed _)     = ""

-- | List of column attributes.
ppColAttrs :: [ColAttr] -> Text
ppColAttrs = T.unwords . map pgColAttr

-- | Hook that allows you to modify 'ppColAttrs' output.
ppColAttrsHook :: SqlTypeRep -> [ColAttr] -> ([ColAttr] -> Text) -> Text
ppColAttrsHook ty attrs fun
    | isGenericIntPrimaryKey ty attrs = fun [AutoPrimary Strong]
    | otherwise = fun attrs

-- | The value used for the next value for an auto-incrementing column.
--   For instance, @DEFAULT@ for PostgreSQL, and @NULL@ for PostgreSQL.
ppAutoIncInsert :: Text
ppAutoIncInsert = "DEFAULT"

-- | Insert queries may have at most this many parameters; if an insertion
--   has more parameters than this, it will be chunked.
--
--   Note that only insertions of multiple rows are chunked. If your table
--   has more than this many columns, you should really rethink
--   your database design.
ppMaxInsertParams :: Maybe Int
ppMaxInsertParams = Nothing

-- | @CREATE INDEX@ suffix to indicate that the index should use the given
--   index method.
ppIndexMethodHook :: IndexMethod -> Text
ppIndexMethodHook = (" USING " <>) . compileIndexMethod
  where
    compileIndexMethod BTreeIndex = "btree"
    compileIndexMethod HashIndex  = "hash"
