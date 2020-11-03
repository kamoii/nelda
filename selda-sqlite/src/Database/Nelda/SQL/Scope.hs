{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Scope where

import Database.Nelda.SQL.Col (Col)
import Database.Nelda.SQL.Nullability
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

-- | Convert one or more inner column to equivalent columns in the outer query.
--   @OuterCols (Aggr (Inner s) a :*: Aggr (Inner s) b) = Col s a :*: Col s b@,
--   for instance.

-- * OuterCols

{-
Selda の元々の実装では以下のような type faily を使っていた。
これには問題があり true :: Col s Bool のように任意の s が free な結果を JOIN の
内側の Query が返した場合, OuterCols の計算において一番目に該当するかが決定でいないため,
計算がそこで止まってしまう。型は OuterCols (Col s0 Bool) になったままである。

type family OuterCols a where
    OuterCols (Col (Inner s) n a) = Col s n a
    OuterCols (Row (Inner s) n a) = Row s n a
    ...

type class + functional dependency によって解決できる。

e.g. instance ( s ~ Innser s' ..) =>  Foo (Col s n a) (Col s' n a)

2.upto(8) do |n|
  vars = (0...n).map{|i| "v#{i}"}
  ress = (0...n).map{|i| "v#{i}'"}
  cons = (0...n).map{|i| "ToOuterCols v#{i} v#{i}'"}
  puts "instance (#{cons.join(', ')}) => ToOuterCols (#{vars.join(', ')}) (#{ress.join(', ')})"
end
-}

class ToOuterCols inner outer | inner -> outer

-- TODO: s ~ Inner s' でない場合に適切なエラーメッセージ
-- TODO: その他の HEAD の場合に適切なエラーメッセージ
instance (s ~ Inner s') => ToOuterCols (Col s n a) (Col s' n a)
instance (s ~ Inner s') => ToOuterCols (Row s n a) (Row s' n a)
instance (ToOuterCols v0 v0', ToOuterCols v1 v1') => ToOuterCols (v0, v1) (v0', v1')
instance (ToOuterCols v0 v0', ToOuterCols v1 v1', ToOuterCols v2 v2') => ToOuterCols (v0, v1, v2) (v0', v1', v2')
instance (ToOuterCols v0 v0', ToOuterCols v1 v1', ToOuterCols v2 v2', ToOuterCols v3 v3') => ToOuterCols (v0, v1, v2, v3) (v0', v1', v2', v3')
instance (ToOuterCols v0 v0', ToOuterCols v1 v1', ToOuterCols v2 v2', ToOuterCols v3 v3', ToOuterCols v4 v4') => ToOuterCols (v0, v1, v2, v3, v4) (v0', v1', v2', v3', v4')
instance (ToOuterCols v0 v0', ToOuterCols v1 v1', ToOuterCols v2 v2', ToOuterCols v3 v3', ToOuterCols v4 v4', ToOuterCols v5 v5') => ToOuterCols (v0, v1, v2, v3, v4, v5) (v0', v1', v2', v3', v4', v5')
instance (ToOuterCols v0 v0', ToOuterCols v1 v1', ToOuterCols v2 v2', ToOuterCols v3 v3', ToOuterCols v4 v4', ToOuterCols v5 v5', ToOuterCols v6 v6') => ToOuterCols (v0, v1, v2, v3, v4, v5, v6) (v0', v1', v2', v3', v4', v5', v6')
instance (ToOuterCols v0 v0', ToOuterCols v1 v1', ToOuterCols v2 v2', ToOuterCols v3 v3', ToOuterCols v4 v4', ToOuterCols v5 v5', ToOuterCols v6 v6', ToOuterCols v7 v7') => ToOuterCols (v0, v1, v2, v3, v4, v5, v6, v7) (v0', v1', v2', v3', v4', v5', v6', v7')

-- {-
-- 2.upto(8) do |n|
--   vars = (0...n).map{|i| "v#{i}"}
--   outs = vars.map {|v| "OuterCols #{v}"}
--   puts "    OuterCols (#{vars.join(',')}) = (#{outs.join(',')})"
-- end
-- -}
-- type family OuterCols a where
--     OuterCols (Col (Inner s) n a) = Col s n a
--     OuterCols (Row (Inner s) n a) = Row s n a
--     OuterCols (v0, v1) = (OuterCols v0, OuterCols v1)
--     OuterCols (v0, v1, v2) = (OuterCols v0, OuterCols v1, OuterCols v2)
--     OuterCols (v0, v1, v2, v3) = (OuterCols v0, OuterCols v1, OuterCols v2, OuterCols v3)
--     OuterCols (v0, v1, v2, v3, v4) = (OuterCols v0, OuterCols v1, OuterCols v2, OuterCols v3, OuterCols v4)
--     OuterCols (v0, v1, v2, v3, v4, v5) = (OuterCols v0, OuterCols v1, OuterCols v2, OuterCols v3, OuterCols v4, OuterCols v5)
--     OuterCols (v0, v1, v2, v3, v4, v5, v6) = (OuterCols v0, OuterCols v1, OuterCols v2, OuterCols v3, OuterCols v4, OuterCols v5, OuterCols v6)
--     OuterCols (v0, v1, v2, v3, v4, v5, v6, v7) = (OuterCols v0, OuterCols v1, OuterCols v2, OuterCols v3, OuterCols v4, OuterCols v5, OuterCols v6, OuterCols v7)
-- -- OuterCols (Col (Inner s) a :*: b) = Col s a :*: OuterCols b
-- -- OuterCols (Row (Inner s) a :*: b) = Row s a :*: OuterCols b
--     OuterCols (Col _s _n _a) =
--         TL.TypeError
--             ( 'TL.Text "An inner query can only return rows and columns from its own scope."
--             )
--     OuterCols (Row _s _n _a) =
--         TL.TypeError
--             ( 'TL.Text "An inner query can only return rows and columns from its own scope."
--             )
--     OuterCols _a =
--         TL.TypeError
--             ( 'TL.Text "Only (inductive tuples of) row and columns can be returned from"
--                 'TL.:$$: 'TL.Text "an inner query."
--             )

-- * LeftCols

-- | The results of a left join are always nullable, as there is no guarantee
--   that all joined columns will be non-null.
--   @JoinCols a@ where @a@ is an extensible tuple is that same tuple, but in
--   the outer query and with all elements nullable.
--   For instance:
--
-- >  LeftCols (Col (Inner s) Int :*: Col (Inner s) Text)
-- >    = Col s (Maybe Int) :*: Col s (Maybe Text)

{-
2.upto(8) do |n|
  vars = (0...n).map{|i| "v#{i}"}
  outs = vars.map {|v| "LeftCols #{v}"}
  puts "    LeftCols (#{vars.join(',')}) = (#{outs.join(',')})"
end
-}
type family LeftCols a where
    LeftCols (Col (Inner s) _ a) = Col s 'Nullable a
    LeftCols (Row (Inner s) _ a) = Row s 'Nullable a
    LeftCols (v0, v1) = (LeftCols v0, LeftCols v1)
    LeftCols (v0, v1, v2) = (LeftCols v0, LeftCols v1, LeftCols v2)
    LeftCols (v0, v1, v2, v3) = (LeftCols v0, LeftCols v1, LeftCols v2, LeftCols v3)
    LeftCols (v0, v1, v2, v3, v4) = (LeftCols v0, LeftCols v1, LeftCols v2, LeftCols v3, LeftCols v4)
    LeftCols (v0, v1, v2, v3, v4, v5) = (LeftCols v0, LeftCols v1, LeftCols v2, LeftCols v3, LeftCols v4, LeftCols v5)
    LeftCols (v0, v1, v2, v3, v4, v5, v6) = (LeftCols v0, LeftCols v1, LeftCols v2, LeftCols v3, LeftCols v4, LeftCols v5, LeftCols v6)
    LeftCols (v0, v1, v2, v3, v4, v5, v6, v7) = (LeftCols v0, LeftCols v1, LeftCols v2, LeftCols v3, LeftCols v4, LeftCols v5, LeftCols v6, LeftCols v7)
-- LeftCols (Col (Inner s) a :*: b) = Col s (CoalesceMaybe (Maybe a)) :*: LeftCols b
-- LeftCols (Row (Inner s) a :*: b) = Row s (CoalesceMaybe (Maybe a)) :*: LeftCols b
    LeftCols _a =
        TL.TypeError
            ( 'TL.Text "Only (inductive tuples of) rows and columns can be returned"
                'TL.:$$: 'TL.Text "from a join."
            )
