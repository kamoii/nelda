{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.Schema.IsColumnNames
    ( module Database.Nelda.Schema.IsColumnNames
    , module Data.Single
    ) where

import Database.Nelda.Schema.Column.Types (TaggedLabel, Tagged(Tagged), ColumnName)
import GHC.TypeLits (Symbol)
import Data.Single (Single(Single))

{-
-- TODO: ColumnName に限らず汎用化できるな...

Ruby script to generate IsColumnNames tuple instances.

def n_tuple_instance(n)
  constraint = n.times.map{ |i| "c#{i} ~ Tagged ColumnName (n#{i} :: Symbol)" }.join(',')
  vars = n.times.map{ |i| "c#{i}" }.join(',')
  vars_tagged_label = n.times.map{ |i| "TaggedLabel c#{i}" }.join(',')
  vars_tagged = n.times.map{ |i| "Tagged c#{i}" }.join(',')
  <<-EOS
instance (#{constraint}) => IsColumnNames (#{vars}) where
    type ToColumnNamesType (#{vars}) = '[#{vars_tagged_label}]
    toColumnNames (#{vars_tagged}) = [#{vars}]
  EOS
end

2.upto(16) do |n|
  puts n_tuple_instance(n)
end

-}
class IsColumnNames names where
    type ToColumnNamesType names :: [Symbol]
    toColumnNames :: names -> [ColumnName]

-- names には基本 OverloadedLables を使って単一カラムの場合 #foo, 複数カラムの場合 (#foo, #bar) と指定されことを想定している。
-- ただ残念ながら (Tagged ColumnName (n0 :: Symbol)) の instanceを実装して 単一カラムを #foo として指定しても曖昧な型として怒られる。
-- 何故なら #foo 自体が多相的であり、単独では要求される型が決まらないからである。
-- 複数カラムの場合、 (#foo, #bar)、(a, b) instance にマッチしてそこから a ~ Tagged .. でラベルの取るべき型が決まるので問題ない。
-- そのため 単独カラムの場合は Single newypte に包んで指定もらう。そうすることでマッチする instance が決まる。
-- ただ Single 忘れて単独カラムを #foo とかで指定してしまうと、曖昧な型という分かりにくい型エラーが出てしまう。

instance (c0 ~ Tagged ColumnName (n0 :: Symbol)) => IsColumnNames (Single c0) where
    type ToColumnNamesType (Single c0) = '[TaggedLabel c0]
    toColumnNames (Single (Tagged c0)) = [c0]

instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol)) => IsColumnNames (c0,c1) where
    type ToColumnNamesType (c0,c1) = '[TaggedLabel c0,TaggedLabel c1]
    toColumnNames (Tagged c0,Tagged c1) = [c0,c1]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol)) => IsColumnNames (c0,c1,c2) where
    type ToColumnNamesType (c0,c1,c2) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2) = [c0,c1,c2]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol)) => IsColumnNames (c0,c1,c2,c3) where
    type ToColumnNamesType (c0,c1,c2,c3) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3) = [c0,c1,c2,c3]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4) where
    type ToColumnNamesType (c0,c1,c2,c3,c4) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4) = [c0,c1,c2,c3,c4]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5) = [c0,c1,c2,c3,c4,c5]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6) = [c0,c1,c2,c3,c4,c5,c6]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7) = [c0,c1,c2,c3,c4,c5,c6,c7]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8) = [c0,c1,c2,c3,c4,c5,c6,c7,c8]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol),c10 ~ Tagged ColumnName (n10 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol),c10 ~ Tagged ColumnName (n10 :: Symbol),c11 ~ Tagged ColumnName (n11 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol),c10 ~ Tagged ColumnName (n10 :: Symbol),c11 ~ Tagged ColumnName (n11 :: Symbol),c12 ~ Tagged ColumnName (n12 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol),c10 ~ Tagged ColumnName (n10 :: Symbol),c11 ~ Tagged ColumnName (n11 :: Symbol),c12 ~ Tagged ColumnName (n12 :: Symbol),c13 ~ Tagged ColumnName (n13 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12,TaggedLabel c13]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12,Tagged c13) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol),c10 ~ Tagged ColumnName (n10 :: Symbol),c11 ~ Tagged ColumnName (n11 :: Symbol),c12 ~ Tagged ColumnName (n12 :: Symbol),c13 ~ Tagged ColumnName (n13 :: Symbol),c14 ~ Tagged ColumnName (n14 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12,TaggedLabel c13,TaggedLabel c14]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12,Tagged c13,Tagged c14) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14]
instance (c0 ~ Tagged ColumnName (n0 :: Symbol),c1 ~ Tagged ColumnName (n1 :: Symbol),c2 ~ Tagged ColumnName (n2 :: Symbol),c3 ~ Tagged ColumnName (n3 :: Symbol),c4 ~ Tagged ColumnName (n4 :: Symbol),c5 ~ Tagged ColumnName (n5 :: Symbol),c6 ~ Tagged ColumnName (n6 :: Symbol),c7 ~ Tagged ColumnName (n7 :: Symbol),c8 ~ Tagged ColumnName (n8 :: Symbol),c9 ~ Tagged ColumnName (n9 :: Symbol),c10 ~ Tagged ColumnName (n10 :: Symbol),c11 ~ Tagged ColumnName (n11 :: Symbol),c12 ~ Tagged ColumnName (n12 :: Symbol),c13 ~ Tagged ColumnName (n13 :: Symbol),c14 ~ Tagged ColumnName (n14 :: Symbol),c15 ~ Tagged ColumnName (n15 :: Symbol)) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12,TaggedLabel c13,TaggedLabel c14,TaggedLabel c15]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12,Tagged c13,Tagged c14,Tagged c15) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
