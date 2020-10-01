{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.IsColNames where

import Database.Nelda.Types (ColName(..), AnyColName(..))

-- * IsColNames type class and instance
{-
Ruby script to generate IsColNames tuple instances.

def n_tuple_instance(n)
  constraint = n.times.map{ |i| "c#{i} ~ ColName n#{i}" }.join(',')
  vars = n.times.map{ |i| "c#{i}" }.join(',')
  vals = n.times.map{ |i| "AnyColName c#{i}" }.join(',')
  <<-EOS
instance (#{constraint}) => IsColNames (#{vars}) where
    type ToColNamesType (#{vars}) = '[#{vars}]
    toAnyColNames (#{vars}) = [#{vals}]
  EOS
end

2.upto(16) do |n|
  puts n_tuple_instance(n)
end

-}
class IsColNames names where
    type ToColNamesType names :: [*]
    toAnyColNames :: names -> [AnyColName]

instance (c0 ~ ColName n0,c1 ~ ColName n1) => IsColNames (c0,c1) where
    type ToColNamesType (c0,c1) = '[c0,c1]
    toAnyColNames (c0,c1) = [AnyColName c0,AnyColName c1]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2) => IsColNames (c0,c1,c2) where
    type ToColNamesType (c0,c1,c2) = '[c0,c1,c2]
    toAnyColNames (c0,c1,c2) = [AnyColName c0,AnyColName c1,AnyColName c2]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3) => IsColNames (c0,c1,c2,c3) where
    type ToColNamesType (c0,c1,c2,c3) = '[c0,c1,c2,c3]
    toAnyColNames (c0,c1,c2,c3) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4) => IsColNames (c0,c1,c2,c3,c4) where
    type ToColNamesType (c0,c1,c2,c3,c4) = '[c0,c1,c2,c3,c4]
    toAnyColNames (c0,c1,c2,c3,c4) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5) => IsColNames (c0,c1,c2,c3,c4,c5) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5) = '[c0,c1,c2,c3,c4,c5]
    toAnyColNames (c0,c1,c2,c3,c4,c5) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6) => IsColNames (c0,c1,c2,c3,c4,c5,c6) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6) = '[c0,c1,c2,c3,c4,c5,c6]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7) = '[c0,c1,c2,c3,c4,c5,c6,c7]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9,c10 ~ ColName n10) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9,AnyColName c10]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9,c10 ~ ColName n10,c11 ~ ColName n11) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9,AnyColName c10,AnyColName c11]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9,c10 ~ ColName n10,c11 ~ ColName n11,c12 ~ ColName n12) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9,AnyColName c10,AnyColName c11,AnyColName c12]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9,c10 ~ ColName n10,c11 ~ ColName n11,c12 ~ ColName n12,c13 ~ ColName n13) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9,AnyColName c10,AnyColName c11,AnyColName c12,AnyColName c13]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9,c10 ~ ColName n10,c11 ~ ColName n11,c12 ~ ColName n12,c13 ~ ColName n13,c14 ~ ColName n14) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9,AnyColName c10,AnyColName c11,AnyColName c12,AnyColName c13,AnyColName c14]
instance (c0 ~ ColName n0,c1 ~ ColName n1,c2 ~ ColName n2,c3 ~ ColName n3,c4 ~ ColName n4,c5 ~ ColName n5,c6 ~ ColName n6,c7 ~ ColName n7,c8 ~ ColName n8,c9 ~ ColName n9,c10 ~ ColName n10,c11 ~ ColName n11,c12 ~ ColName n12,c13 ~ ColName n13,c14 ~ ColName n14,c15 ~ ColName n15) => IsColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) where
    type ToColNamesType (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) = '[c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
    toAnyColNames (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15) = [AnyColName c0,AnyColName c1,AnyColName c2,AnyColName c3,AnyColName c4,AnyColName c5,AnyColName c6,AnyColName c7,AnyColName c8,AnyColName c9,AnyColName c10,AnyColName c11,AnyColName c12,AnyColName c13,AnyColName c14,AnyColName c15]
