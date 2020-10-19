{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.Schema.IsColumnNames where


import Database.Nelda.Schema.Column.Types (ColumnName, AnyColumnName(AnyColumnName))

-- * IsColumnNames type class and instance
{-
Ruby script to generate IsColumnNames tuple instances.

def n_tuple_instance(n)
  constraint = n.times.map{ |i| "c#{i} ~ ColumnName n#{i}" }.join(',')
  vars = n.times.map{ |i| "c#{i}" }.join(',')
  vals = n.times.map{ |i| "AnyColumnName c#{i}" }.join(',')
  <<-EOS
instance (#{constraint}) => IsColumnNames (#{vars}) where
    type ToColumnNamesType (#{vars}) = '[#{vars}]
    toAnyColumnNames (#{vars}) = [#{vals}]
  EOS
end

2.upto(16) do |n|
  puts n_tuple_instance(n)
end

-}
class IsColumnNames names where
    type ToColumnNamesType names :: [*]
    toAnyColumnNames :: names -> [AnyColumnName]

instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1) => IsColumnNames (c0,c1) where
    type ToColumnNamesType (c0,c1) = '[c0,c1]
    toAnyColumnNames (c0,c1) = [AnyColumnName c0,AnyColumnName c1]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2) => IsColumnNames (c0,c1,c2) where
    type ToColumnNamesType (c0,c1,c2) = '[c0,c1,c2]
    toAnyColumnNames (c0,c1,c2) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3) => IsColumnNames (c0,c1,c2,c3) where
    type ToColumnNamesType (c0,c1,c2,c3) = '[c0,c1,c2,c3]
    toAnyColumnNames (c0,c1,c2,c3) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4) => IsColumnNames (c0,c1,c2,c3,c4) where
    type ToColumnNamesType (c0,c1,c2,c3,c4) = '[c0,c1,c2,c3,c4]
    toAnyColumnNames (c0,c1,c2,c3,c4) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5) => IsColumnNames (c0,c1,c2,c3,c4,c5) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5) = '[c0,c1,c2,c3,c4,c5]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6) = '[c0,c1,c2,c3,c4,c5,c6]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7) = '[c0,c1,c2,c3,c4,c5,c6,c7]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9,c10 ~ ColumnName n10) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9,AnyColumnName c10]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9,c10 ~ ColumnName n10,c11 ~ ColumnName n11) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9,AnyColumnName c10,AnyColumnName c11]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9,c10 ~ ColumnName n10,c11 ~ ColumnName n11,c12 ~ ColumnName n12) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9,AnyColumnName c10,AnyColumnName c11,AnyColumnName c12]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9,c10 ~ ColumnName n10,c11 ~ ColumnName n11,c12 ~ ColumnName n12,c13 ~ ColumnName n13) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9,AnyColumnName c10,AnyColumnName c11,AnyColumnName c12,AnyColumnName c13]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9,c10 ~ ColumnName n10,c11 ~ ColumnName n11,c12 ~ ColumnName n12,c13 ~ ColumnName n13,c14 ~ ColumnName n14) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9,AnyColumnName c10,AnyColumnName c11,AnyColumnName c12,AnyColumnName c13,AnyColumnName c14]
instance (c0 ~ ColumnName n0,c1 ~ ColumnName n1,c2 ~ ColumnName n2,c3 ~ ColumnName n3,c4 ~ ColumnName n4,c5 ~ ColumnName n5,c6 ~ ColumnName n6,c7 ~ ColumnName n7,c8 ~ ColumnName n8,c9 ~ ColumnName n9,c10 ~ ColumnName n10,c11 ~ ColumnName n11,c12 ~ ColumnName n12,c13 ~ ColumnName n13,c14 ~ ColumnName n14,c15 ~ ColumnName n15) => IsColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) where
    type ToColumnNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
    toAnyColumnNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) = [AnyColumnName c0,AnyColumnName c1,AnyColumnName c2,AnyColumnName c3,AnyColumnName c4,AnyColumnName c5,AnyColumnName c6,AnyColumnName c7,AnyColumnName c8,AnyColumnName c9,AnyColumnName c10,AnyColumnName c11,AnyColumnName c12,AnyColumnName c13,AnyColumnName c14,AnyColumnName c15]
