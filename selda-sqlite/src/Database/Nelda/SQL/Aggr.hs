{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Aggr where

import Data.Text (Text)
import Data.Tup
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Scope (Inner)
import Database.Nelda.SQL.Types (Exp (AggrEx), UntypedCol (Untyped))
import Database.Nelda.SqlType (SqlType)
import qualified GHC.TypeLits as TL

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
aggr f (One x) = Aggr (AggrEx f x)

type family AggrCols a where
    AggrCols (Aggr (Inner s) a :*: b) = Col s a :*: AggrCols b
    AggrCols (Aggr (Inner s) a) = Col s a
    AggrCols (Aggr _s _a) =
        TL.TypeError
            ( 'TL.Text "An aggregate query can only return columns from its own"
                'TL.:$$: 'TL.Text "scope."
            )
    AggrCols _a =
        TL.TypeError
            ( 'TL.Text "Only (inductive tuples of) aggregates can be returned from"
                'TL.:$$: 'TL.Text "an aggregate query."
            )

-- | One or more aggregate columns.
class Aggregates a where
    unAggrs :: a -> [UntypedCol]

instance Aggregates (Aggr (Inner s) a) where
    unAggrs (Aggr x) = [Untyped x]
instance Aggregates b => Aggregates (Aggr (Inner s) a :*: b) where
    unAggrs (Aggr a :*: b) = Untyped a : unAggrs b
