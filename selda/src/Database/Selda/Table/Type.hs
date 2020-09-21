module Database.Selda.Table.Type
    ( module Database.Selda.Table.Type
    , module Database.Selda.Backend.PPConfig
    ) where
import Database.Selda.SqlType (SqlTypeRep)
import Database.Selda.Backend.PPConfig
import Database.Selda.SQL
import Database.Selda.Types

-- | A database table, based on some Haskell data type.
--   Any single constructor type can form the basis of a table, as long as
--   it derives @Generic@ and all of its fields are instances of @SqlType@.
data Table a = Table
  { -- | Name of the table. NOT guaranteed to be a valid SQL name.
    tableName :: TableName

    -- | All table columns.
    --   Invariant: the 'colAttrs' list of each column is sorted and contains
    --   no duplicates.
  , tableCols :: [ColInfo]

    -- | Does the given table have an auto-incrementing primary key?
  , tableHasAutoPK :: Bool

    -- | Attributes involving multiple columns.
  , tableAttrs :: [([Int], ColAttr)]
  }

-- | A complete description of a database column.
data ColInfo = ColInfo
  { colName  :: ColName
  , colType  :: SqlTypeRep
  , colAttrs :: [ColAttr]
  , colFKs   :: [(Table (), ColName)]
  , colExpr  :: UntypedCol
  }

isAutoPrimary :: ColAttr -> Bool
isAutoPrimary (AutoPrimary _) = True
isAutoPrimary _               = False

isPrimary :: ColAttr -> Bool
isPrimary Primary = True
isPrimary attr    = isAutoPrimary attr

isUnique :: ColAttr -> Bool
isUnique Unique      = True
isUnique (Indexed _) = True
isUnique attr        = isPrimary attr
