{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Database.Nelda.SQL.Aggr where

import Database.Nelda.SQL.Types (UntypedCol(Untyped), UntypedCol, Exp(AggrEx))
import Database.Nelda.SQL.Col (Col(One), Col)
import Database.Nelda.SqlType (SqlType)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Tup
import qualified GHC.TypeLits as TL
import Database.Nelda.SQL.Row (Row)
import GHC.TypeLits (ErrorMessage((:$$:)))

-- | A single aggregate column.
--   Aggregate columns may not be used to restrict queries.
--   When returned from an 'aggregate' subquery, an aggregate column is
--   converted into a non-aggregate column.
newtype Aggr s a = Aggr {unAggr :: Exp a}

-- | Lift a function over columns to aggregates.
liftAggr :: (Col s a -> Col s b) -> Aggr s a -> Aggr s b
liftAggr f = Aggr . unOne . f . One . unAggr
  where
    unOne (One x) = x

-- | Create a named aggregate function.
--   Like 'fun', this function is generally unsafe and should ONLY be used
--   to implement missing backend-specific functionality.
aggr :: SqlType a => Text -> Col s a -> Aggr s b
aggr f (One x)  = Aggr (AggrEx f x)

-- | Denotes an inner query.
--   For aggregation, treating sequencing as the cartesian product of queries
--   does not work well.
--   Instead, we treat the sequencing of 'aggregate' with other
--   queries as the cartesian product of the aggregated result of the query,
--   a small but important difference.
--
--   However, for this to work, the aggregate query must not depend on any
--   columns in the outer product. Therefore, we let the aggregate query be
--   parameterized over @Inner s@ if the parent query is parameterized over @s@,
--   to enforce this separation.
data Inner s
    deriving Typeable

-- | Convert one or more inner column to equivalent columns in the outer query.
--   @OuterCols (Aggr (Inner s) a :*: Aggr (Inner s) b) = Col s a :*: Col s b@,
--   for instance.
type family OuterCols a where
    OuterCols (Col (Inner s) a :*: b)  = Col s a :*: OuterCols b
    OuterCols (Col (Inner s) a)        = Col s a
    OuterCols (Row (Inner s) a :*: b)  = Row s a :*: OuterCols b
    OuterCols (Row (Inner s) a)        = Row s a
    OuterCols (Col _s _a) = TL.TypeError
      ( 'TL.Text "An inner query can only return rows and columns from its own scope."
      )
    OuterCols (Row _s _a) = TL.TypeError
      ( 'TL.Text "An inner query can only return rows and columns from its own scope."
      )
    OuterCols _a = TL.TypeError
      ( 'TL.Text "Only (inductive tuples of) row and columns can be returned from" ':$$:
        'TL.Text "an inner query."
      )

type family AggrCols a where
    AggrCols (Aggr (Inner s) a :*: b) = Col s a :*: AggrCols b
    AggrCols (Aggr (Inner s) a)       = Col s a
    AggrCols (Aggr _s _a) = TL.TypeError
      ( 'TL.Text "An aggregate query can only return columns from its own" ':$$:
        'TL.Text "scope."
      )
    AggrCols _a = TL.TypeError
      ( 'TL.Text "Only (inductive tuples of) aggregates can be returned from" ':$$:
        'TL.Text "an aggregate query."
      )

-- | The results of a left join are always nullable, as there is no guarantee
--   that all joined columns will be non-null.
--   @JoinCols a@ where @a@ is an extensible tuple is that same tuple, but in
--   the outer query and with all elements nullable.
--   For instance:
--
-- >  LeftCols (Col (Inner s) Int :*: Col (Inner s) Text)
-- >    = Col s (Maybe Int) :*: Col s (Maybe Text)
type family LeftCols a where
    LeftCols (Col (Inner s) (Maybe a) :*: b) = Col s (Maybe a) :*: LeftCols b
    LeftCols (Col (Inner s) a :*: b)         = Col s (Maybe a) :*: LeftCols b
    LeftCols (Col (Inner s) (Maybe a))       = Col s (Maybe a)
    LeftCols (Col (Inner s) a)               = Col s (Maybe a)
    LeftCols (Row (Inner s) (Maybe a) :*: b) = Row s (Maybe a) :*: LeftCols b
    LeftCols (Row (Inner s) a :*: b)         = Row s (Maybe a) :*: LeftCols b
    LeftCols (Row (Inner s) (Maybe a))       = Row s (Maybe a)
    LeftCols (Row (Inner s) a)               = Row s (Maybe a)
    LeftCols _a = TL.TypeError
      ( 'TL.Text "Only (inductive tuples of) rows and columns can be returned" ':$$:
        'TL.Text "from a join."
      )

-- | One or more aggregate columns.
class Aggregates a where
    unAggrs :: a -> [UntypedCol]
instance Aggregates (Aggr (Inner s) a) where
    unAggrs (Aggr x) = [Untyped x]
instance Aggregates b => Aggregates (Aggr (Inner s) a :*: b) where
    unAggrs (Aggr a :*: b) = Untyped a : unAggrs b
