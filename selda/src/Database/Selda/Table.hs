{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs, CPP, DeriveGeneric, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Database.Selda.Table
  ( SelectorGroup, Attr (..), Table (..), Attribute
  , ColInfo (..), ColAttr (..), IndexMethod (..)
  , ForeignKey (..)
  , table, tableFieldMod
  , primary, autoPrimary, untypedAutoPrimary, unique
  , index, indexUsing
  , tableExpr
  ) where
import Data.Text (Text)
#if MIN_VERSION_base(4, 10, 0)
import Data.Typeable
#else
import Data.Proxy
#endif
import Database.Selda.Types
import Database.Selda.Selectors
import Database.Selda.SqlType
import Database.Selda.Column (Row (..))
import Database.Selda.Generic
import Database.Selda.Table.Type
import Database.Selda.Table.Validation (snub)

-- | A group of one or more selectors.
--   A selector group is either a selector (i.e. @#id@), or an inductive
--   tuple of selectors (i.e. @#foo :*: #bar@).
class SelectorGroup t a where
  indices :: Proxy t -> a -> [Int]

instance SelectorGroup t (Selector t a) where
  indices _ s = [selectorIndex s]

instance SelectorGroup t b => SelectorGroup t (Selector t a :*: b) where
  indices p (s :*: ss) = selectorIndex s : indices p ss

-- | A generic column attribute.
--   Essentially a pair or a record selector over the type @a@ and a column
--   attribute.
data Attr a where
  (:-) :: SelectorGroup t a => a -> Attribute t a -> Attr t

-- | Generate a table from the given table name and list of column attributes.
--   All @Maybe@ fields in the table's type will be represented by nullable
--   columns, and all non-@Maybe@ fields fill be represented by required
--   columns.
--   For example:
--
-- > data Person = Person
-- >   { id   :: ID Person
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: Table Person
-- > people = table "people" [pId :- autoPrimary]
-- > pId :*: pName :*: pAge :*: pPet = selectors people
--
--   This will result in a table of @Person@s, with an auto-incrementing primary
--   key.
--
--   If the given type does not have record selectors, the column names will be
--   @col_1@, @col_2@, etc.
table :: forall a. Relational a
         => TableName
         -> [Attr a]
         -> Table a
table tn attrs = tableFieldMod tn attrs id

-- | Generate a table from the given table name,
--   a list of column attributes and a function
--   that maps from field names to column names.
--   Ex.:
--
-- > data Person = Person
-- >   { personId   :: Int
-- >   , personName :: Text
-- >   , personAge  :: Int
-- >   , personPet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: Table Person
-- > people = tableFieldMod "people" [personName :- autoPrimaryGen] (fromJust . stripPrefix "person")
--
--   This will create a table with the columns named
--   @Id@, @Name@, @Age@ and @Pet@.
tableFieldMod :: forall a. Relational a
                 => TableName
                 -> [Attr a]
                 -> (Text -> Text)
                 -> Table a
tableFieldMod tn attrs fieldMod = Table
  { tableName = tn
  , tableCols = map tidy cols
  , tableHasAutoPK = apk
  , tableAttrs = combinedAttrs
  }
  where
    combinedAttrs =
      [ (ixs, a)
      | sel :- Attribute [a] <- attrs
      , let ixs = indices (Proxy @a) sel
      , case ixs of
          []  -> False
          [_] -> False
          _   -> True
      ]
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a) fieldMod)
    apk = or [AutoIncrement `elem` as | _ :- Attribute as <- attrs]
    addAttrs n ci = ci
      { colAttrs = colAttrs ci ++ concat
          [ as
          | sel :- Attribute as <- attrs
          , case indices (Proxy @a) sel of
              [colIx] -> colIx == n
              _       -> False
          ]
      , colFKs = colFKs ci ++
          [ thefk
          | sel :- ForeignKey thefk <- attrs
          , case indices (Proxy @a) sel of
              [colIx] -> colIx == n
              _       -> False
          ]
      }

-- | Remove duplicate attributes.
tidy :: ColInfo -> ColInfo
tidy ci = ci {colAttrs = snub $ colAttrs ci}

-- | Some attribute that may be set on a column of type @c@, in a table of
--   type @t@.
data Attribute t c
  = Attribute [ColAttr]
  | ForeignKey (Table (), ColName)

-- | A primary key which does not auto-increment.
primary :: Attribute t (Selector t c)
primary = Attribute [Primary, Required, Unique]

-- | Create an index on this column.
index :: Attribute t (Selector t c)
index = Attribute [Indexed Nothing]

-- | Create an index using the given index method on this column.
indexUsing :: IndexMethod -> Attribute t (Selector t c)
indexUsing m = Attribute [Indexed (Just m)]

-- | An auto-incrementing primary key.
autoPrimary :: Attribute t (Selector t (ID t))
autoPrimary = Attribute [Primary, AutoIncrement, Required, Unique]

-- | An untyped auto-incrementing primary key.
--   You should really only use this for ad hoc tables, such as tuples.
untypedAutoPrimary :: Attribute t (Selector t RowID)
untypedAutoPrimary = Attribute [Primary, AutoIncrement, Required, Unique]

-- | A table-unique value.
unique :: SelectorGroup t g => Attribute t g
unique = Attribute [Unique]

mkFK :: Table t -> Selector a b -> Attribute c (Selector c d)
mkFK (Table tn tcs tapk tas) sel =
  ForeignKey (Table tn tcs tapk tas, colName (tcs !! selectorIndex sel))

class ForeignKey a b where
  -- | A foreign key constraint referencing the given table and column.
  foreignKey :: Table t -> Selector t a -> Attribute self (Selector self b)

instance ForeignKey a a where
  foreignKey = mkFK
instance ForeignKey (Maybe a) a where
  foreignKey = mkFK
instance ForeignKey a (Maybe a) where
  foreignKey = mkFK

-- | An expression representing the given table.
tableExpr :: Table a -> Row s a
tableExpr = Many . map colExpr . tableCols
