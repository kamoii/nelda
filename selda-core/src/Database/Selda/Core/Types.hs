{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Selda.Core.Types where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.String (IsString)

-- | Uniquely identifies some particular backend.
--
--   When publishing a new backend, consider submitting a pull request with a
--   constructor for your backend instead of using the @Other@ constructor.
data BackendID = SQLite | PostgreSQL | Other Text
  deriving (Show, Eq, Ord)

-- | Thrown by any function in 'SeldaT' if an error occurs.
data SeldaError
  = DbError String     -- ^ Unable to open or connect to database.
  | SqlError String    -- ^ An error occurred while executing query.
  | UnsafeError String -- ^ An error occurred due to improper use of an unsafe
                       --   function.
  deriving (Show, Eq, Typeable)

instance Exception SeldaError

-- | A prepared statement identifier. Guaranteed to be unique per application.
newtype StmtID = StmtID Int
  deriving (Show, Eq, Ord)

-- | A connection identifier. Guaranteed to be unique per application.
newtype ConnID = ConnID Int
  deriving (Show, Eq, Ord)

-- | Name of a database column.
newtype ColName = ColName { unColName :: Text }
  deriving (Ord, Eq, Show, IsString)

-- | Name of a database table.
newtype TableName = TableName Text
  deriving (Ord, Eq, Show, IsString)

-- | Comprehensive information about a table.
data TableInfo sqlTypeRep = TableInfo
  { -- | Ordered information about each table column.
    tableColumnInfos :: [ColumnInfo sqlTypeRep]
    -- | Unordered list of all (non-PK) uniqueness constraints on this table.
  , tableUniqueGroups :: [[ColName]]
    -- | Unordered list of all primary key constraints on this table.
  , tablePrimaryKey :: [ColName]
  } deriving (Show, Eq)

-- | Comprehensive information about a column.
data ColumnInfo sqlTypeRep = ColumnInfo
  { -- | Name of the column.
    colName :: ColName
    -- | Selda type of the column, or the type name given by the database
    --   if Selda couldn't make sense of the type.
  , colType :: Either Text sqlTypeRep
    -- | Is the given column auto-incrementing?
  , colIsAutoPrimary :: Bool
    -- | Can the column be NULL?
  , colIsNullable :: Bool
    -- | Is the column explicitly indexed (i.e. using 'indexed')?
  , colHasIndex :: Bool
    -- | Any foreign key (table, column) pairs referenced by this column.
  , colFKs :: [(TableName, ColName)]
  } deriving (Show, Eq)
