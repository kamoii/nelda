{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.Schema.IsColumnNames where

import Database.Nelda.Schema.Column.Types (TaggedLabel, Tagged(Tagged), ColumnName)
import GHC.TypeLits (Symbol)

{-
-- TODO: ColumnName に限らず汎用化できるな...

Ruby script to generate IsColumnNames tuple instances.

def n_tuple_instance(n)
  constraint = n.times.map{ |i| "c#{i} ~ Tagged ColumnName n#{i}" }.join(',')
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

instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1) => IsColumnNames (c0,c1) where
    type ToColumnNamesType (c0,c1) = '[TaggedLabel c0,TaggedLabel c1]
    toColumnNames (Tagged c0,Tagged c1) = [c0,c1]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2) => IsColumnNames (c0,c1,c2) where
    type ToColumnNamesType (c0,c1,c2) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2) = [c0,c1,c2]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3) => IsColumnNames (c0,c1,c2,c3) where
    type ToColumnNamesType (c0,c1,c2,c3) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3) = [c0,c1,c2,c3]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4) => IsColumnNames (c0,c1,c2,c3,c4) where
    type ToColumnNamesType (c0,c1,c2,c3,c4) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4) = [c0,c1,c2,c3,c4]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5) => IsColumnNames (c0,c1,c2,c3,c4,c5) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5) = [c0,c1,c2,c3,c4,c5]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6) = [c0,c1,c2,c3,c4,c5,c6]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7) = [c0,c1,c2,c3,c4,c5,c6,c7]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8) = [c0,c1,c2,c3,c4,c5,c6,c7,c8]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9,c10 ~ Tagged ColumnName n10) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9,c10 ~ Tagged ColumnName n10,c11 ~ Tagged ColumnName n11) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9,c10 ~ Tagged ColumnName n10,c11 ~ Tagged ColumnName n11,c12 ~ Tagged ColumnName n12) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9,c10 ~ Tagged ColumnName n10,c11 ~ Tagged ColumnName n11,c12 ~ Tagged ColumnName n12,c13 ~ Tagged ColumnName n13) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12,TaggedLabel c13]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12,Tagged c13) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9,c10 ~ Tagged ColumnName n10,c11 ~ Tagged ColumnName n11,c12 ~ Tagged ColumnName n12,c13 ~ Tagged ColumnName n13,c14 ~ Tagged ColumnName n14) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12,TaggedLabel c13,TaggedLabel c14]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12,Tagged c13,Tagged c14) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14]
instance (c0 ~ Tagged ColumnName n0,c1 ~ Tagged ColumnName n1,c2 ~ Tagged ColumnName n2,c3 ~ Tagged ColumnName n3,c4 ~ Tagged ColumnName n4,c5 ~ Tagged ColumnName n5,c6 ~ Tagged ColumnName n6,c7 ~ Tagged ColumnName n7,c8 ~ Tagged ColumnName n8,c9 ~ Tagged ColumnName n9,c10 ~ Tagged ColumnName n10,c11 ~ Tagged ColumnName n11,c12 ~ Tagged ColumnName n12,c13 ~ Tagged ColumnName n13,c14 ~ Tagged ColumnName n14,c15 ~ Tagged ColumnName n15) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) = '[TaggedLabel c0,TaggedLabel c1,TaggedLabel c2,TaggedLabel c3,TaggedLabel c4,TaggedLabel c5,TaggedLabel c6,TaggedLabel c7,TaggedLabel c8,TaggedLabel c9,TaggedLabel c10,TaggedLabel c11,TaggedLabel c12,TaggedLabel c13,TaggedLabel c14,TaggedLabel c15]
    toColumnNames (Tagged c0,Tagged c1,Tagged c2,Tagged c3,Tagged c4,Tagged c5,Tagged c6,Tagged c7,Tagged c8,Tagged c9,Tagged c10,Tagged c11,Tagged c12,Tagged c13,Tagged c14,Tagged c15) = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
