{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Scope where

import Data.Tup ((:*:))
import Data.Typeable (Typeable)
import Database.Nelda.SQL.Col (Col)
import Database.Nelda.SQL.Row (Row)
import qualified GHC.TypeLits as TL

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
    deriving (Typeable)

-- | Convert one or more inner column to equivalent columns in the outer query.
--   @OuterCols (Aggr (Inner s) a :*: Aggr (Inner s) b) = Col s a :*: Col s b@,
--   for instance.
type family OuterCols a where
    OuterCols (Col (Inner s) a :*: b) = Col s a :*: OuterCols b
    OuterCols (Col (Inner s) a) = Col s a
    OuterCols (Row (Inner s) a :*: b) = Row s a :*: OuterCols b
    OuterCols (Row (Inner s) a) = Row s a
    OuterCols (Col _s _a) =
        TL.TypeError
            ( 'TL.Text "An inner query can only return rows and columns from its own scope."
            )
    OuterCols (Row _s _a) =
        TL.TypeError
            ( 'TL.Text "An inner query can only return rows and columns from its own scope."
            )
    OuterCols _a =
        TL.TypeError
            ( 'TL.Text "Only (inductive tuples of) row and columns can be returned from"
                'TL.:$$: 'TL.Text "an inner query."
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
    LeftCols (Col (Inner s) a :*: b) = Col s (Maybe a) :*: LeftCols b
    LeftCols (Col (Inner s) (Maybe a)) = Col s (Maybe a)
    LeftCols (Col (Inner s) a) = Col s (Maybe a)
    LeftCols (Row (Inner s) (Maybe a) :*: b) = Row s (Maybe a) :*: LeftCols b
    LeftCols (Row (Inner s) a :*: b) = Row s (Maybe a) :*: LeftCols b
    LeftCols (Row (Inner s) (Maybe a)) = Row s (Maybe a)
    LeftCols (Row (Inner s) a) = Row s (Maybe a)
    LeftCols _a =
        TL.TypeError
            ( 'TL.Text "Only (inductive tuples of) rows and columns can be returned"
                'TL.:$$: 'TL.Text "from a join."
            )
