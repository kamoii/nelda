{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Aggr where

import Data.Text (Text)
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

{-
2.upto(8) do |n|
  vars = (0...n).map{|i| "v#{i}"}
  outs = vars.map {|v| "AggrCols #{v}"}
  puts "    AggrCols (#{vars.join(',')}) = (#{outs.join(',')})"
end
-}
type family AggrCols a where
    AggrCols (Aggr (Inner s) a) = Col s a
    AggrCols (v0,v1) = (AggrCols v0,AggrCols v1)
    AggrCols (v0,v1,v2) = (AggrCols v0,AggrCols v1,AggrCols v2)
    AggrCols (v0,v1,v2,v3) = (AggrCols v0,AggrCols v1,AggrCols v2,AggrCols v3)
    AggrCols (v0,v1,v2,v3,v4) = (AggrCols v0,AggrCols v1,AggrCols v2,AggrCols v3,AggrCols v4)
    AggrCols (v0,v1,v2,v3,v4,v5) = (AggrCols v0,AggrCols v1,AggrCols v2,AggrCols v3,AggrCols v4,AggrCols v5)
    AggrCols (v0,v1,v2,v3,v4,v5,v6) = (AggrCols v0,AggrCols v1,AggrCols v2,AggrCols v3,AggrCols v4,AggrCols v5,AggrCols v6)
    AggrCols (v0,v1,v2,v3,v4,v5,v6,v7) = (AggrCols v0,AggrCols v1,AggrCols v2,AggrCols v3,AggrCols v4,AggrCols v5,AggrCols v6,AggrCols v7)
    -- AggrCols (Aggr (Inner s) a :*: b) = Col s a :*: AggrCols b
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

{-
def tuple_instance(n)
  vars = (0...n).map {|i| "c#{i}" }
  constraint = vars.map {|v| "Aggregates #{v}"}.join(',')
  elms = vars.map {|v| "unAggrs #{v}"}.join(' <> ')
  <<EOS
instance (#{constraint}) => Aggregates (#{vars.join(',')}) where
    unAggrs (#{vars.join(',')}) = #{elms}
EOS
end

2.upto(8) { |n| puts tuple_instance(n) }
-}
instance (Aggregates c0,Aggregates c1) => Aggregates (c0,c1) where
    unAggrs (c0,c1) = unAggrs c0 <> unAggrs c1
instance (Aggregates c0,Aggregates c1,Aggregates c2) => Aggregates (c0,c1,c2) where
    unAggrs (c0,c1,c2) = unAggrs c0 <> unAggrs c1 <> unAggrs c2
instance (Aggregates c0,Aggregates c1,Aggregates c2,Aggregates c3) => Aggregates (c0,c1,c2,c3) where
    unAggrs (c0,c1,c2,c3) = unAggrs c0 <> unAggrs c1 <> unAggrs c2 <> unAggrs c3
instance (Aggregates c0,Aggregates c1,Aggregates c2,Aggregates c3,Aggregates c4) => Aggregates (c0,c1,c2,c3,c4) where
    unAggrs (c0,c1,c2,c3,c4) = unAggrs c0 <> unAggrs c1 <> unAggrs c2 <> unAggrs c3 <> unAggrs c4
instance (Aggregates c0,Aggregates c1,Aggregates c2,Aggregates c3,Aggregates c4,Aggregates c5) => Aggregates (c0,c1,c2,c3,c4,c5) where
    unAggrs (c0,c1,c2,c3,c4,c5) = unAggrs c0 <> unAggrs c1 <> unAggrs c2 <> unAggrs c3 <> unAggrs c4 <> unAggrs c5
instance (Aggregates c0,Aggregates c1,Aggregates c2,Aggregates c3,Aggregates c4,Aggregates c5,Aggregates c6) => Aggregates (c0,c1,c2,c3,c4,c5,c6) where
    unAggrs (c0,c1,c2,c3,c4,c5,c6) = unAggrs c0 <> unAggrs c1 <> unAggrs c2 <> unAggrs c3 <> unAggrs c4 <> unAggrs c5 <> unAggrs c6
instance (Aggregates c0,Aggregates c1,Aggregates c2,Aggregates c3,Aggregates c4,Aggregates c5,Aggregates c6,Aggregates c7) => Aggregates (c0,c1,c2,c3,c4,c5,c6,c7) where
    unAggrs (c0,c1,c2,c3,c4,c5,c6,c7) = unAggrs c0 <> unAggrs c1 <> unAggrs c2 <> unAggrs c3 <> unAggrs c4 <> unAggrs c5 <> unAggrs c6 <> unAggrs c7

-- instance Aggregates b => Aggregates (Aggr (Inner s) a :*: b) where
--     unAggrs (Aggr a :*: b) = Untyped a : unAggrs b
