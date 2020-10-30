{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Query.Result where

import Database.Nelda.Query.ResultReader
import Database.Nelda.Query.ResultRow (nextResult, ResultRow)
import Database.Nelda.SqlType
import Database.Nelda.SQL.Types (UntypedCol(Untyped), SomeCol(Some))
import Database.Nelda.SQL.Col (Col(One))
import Database.Nelda.SQL.Row (Row(Many))
import Data.Typeable (Typeable)
import Data.Data (Proxy(Proxy))

-- Query の結果を取り出すための type class
-- Col s a は SqlType を使って, Row s a は ResultRow 型クラスを使う

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Typeable (Res r) => Result r where
    type Res r :: *
    -- | Converts the given list of @SqlValue@s into an tuple of well-typed
    --   results.
    --   See 'querySQLite' for example usage.
    toResult :: Proxy r -> ResultReader (Res r)

    -- | Produce a list of all columns present in the result.
    finalCols :: r -> [SomeCol]

buildResult :: Result r => Proxy r -> [SqlValue] -> Res r
buildResult p = runResultReader (toResult p)

instance SqlType a => Result (Col s a) where
    type Res (Col s a) = a
    toResult _ = fromSqlValue <$> next
    finalCols (One c) = [Some c]

instance ResultRow a => Result (Row s a) where
    type Res (Row s a) = a
    toResult _ = nextResult
    finalCols (Many cs) = [Some c | Untyped c <- cs]

{-
def tuple_instance(n)
  vars = (0...n).map {|i| "c#{i}" }
  constraint = vars.map {|v| "Result #{v}"}.join(',')
  tuples_sec = "(#{vars.map{''}.join(',')})"
  to_tups = vars.map {|v| "_toTup @#{v}"}.join(' <*> ')
  from_tups = vars.map {|v| "_fromTup #{v}"}.join(' *> ')
  <<EOS
instance (#{constraint}) => Result (#{vars.join(',')}) where
    type Res (#{vars.join(',')}) = (#{vars.map{|v| "Res #{v}"}.join(',')})
    toResult _ = #{tuples_sec} <$> #{vars.map{|v| "toResult (Proxy :: Proxy #{v})"}.join(' <*> ')}
    finalCols (#{vars.join(',')}) = #{vars.map{|v| "finalCols #{v}"}.join(' <> ')}
EOS
end

2.upto(8) { |n| puts tuple_instance(n) }
-}
instance (Result c0,Result c1) => Result (c0,c1) where
    type Res (c0,c1) = (Res c0,Res c1)
    toResult _ = (,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1)
    finalCols (c0,c1) = finalCols c0 <> finalCols c1
instance (Result c0,Result c1,Result c2) => Result (c0,c1,c2) where
    type Res (c0,c1,c2) = (Res c0,Res c1,Res c2)
    toResult _ = (,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2)
    finalCols (c0,c1,c2) = finalCols c0 <> finalCols c1 <> finalCols c2
instance (Result c0,Result c1,Result c2,Result c3) => Result (c0,c1,c2,c3) where
    type Res (c0,c1,c2,c3) = (Res c0,Res c1,Res c2,Res c3)
    toResult _ = (,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3)
    finalCols (c0,c1,c2,c3) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3
instance (Result c0,Result c1,Result c2,Result c3,Result c4) => Result (c0,c1,c2,c3,c4) where
    type Res (c0,c1,c2,c3,c4) = (Res c0,Res c1,Res c2,Res c3,Res c4)
    toResult _ = (,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4)
    finalCols (c0,c1,c2,c3,c4) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4
instance (Result c0,Result c1,Result c2,Result c3,Result c4,Result c5) => Result (c0,c1,c2,c3,c4,c5) where
    type Res (c0,c1,c2,c3,c4,c5) = (Res c0,Res c1,Res c2,Res c3,Res c4,Res c5)
    toResult _ = (,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5)
    finalCols (c0,c1,c2,c3,c4,c5) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5
instance (Result c0,Result c1,Result c2,Result c3,Result c4,Result c5,Result c6) => Result (c0,c1,c2,c3,c4,c5,c6) where
    type Res (c0,c1,c2,c3,c4,c5,c6) = (Res c0,Res c1,Res c2,Res c3,Res c4,Res c5,Res c6)
    toResult _ = (,,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5) <*> toResult (Proxy :: Proxy c6)
    finalCols (c0,c1,c2,c3,c4,c5,c6) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5 <> finalCols c6
instance (Result c0,Result c1,Result c2,Result c3,Result c4,Result c5,Result c6,Result c7) => Result (c0,c1,c2,c3,c4,c5,c6,c7) where
    type Res (c0,c1,c2,c3,c4,c5,c6,c7) = (Res c0,Res c1,Res c2,Res c3,Res c4,Res c5,Res c6,Res c7)
    toResult _ = (,,,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5) <*> toResult (Proxy :: Proxy c6) <*> toResult (Proxy :: Proxy c7)
    finalCols (c0,c1,c2,c3,c4,c5,c6,c7) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5 <> finalCols c6 <> finalCols c7

-- instance (SqlType a, Result b) => Result (Col s a :*: b) where
--     type Res (Col s a :*: b) = a :*: Res b
--     toResult _ = liftM2 (:*:) (fromSqlValue <$> next) (toResult (Proxy :: Proxy b))
--     finalCols (a :*: b) = finalCols a ++ finalCols b

-- instance (ResultRow a, Result b) => Result (Row s a :*: b) where
--     type Res (Row s a :*: b) = a :*: Res b
--     toResult _ = liftM2 (:*:) nextResult (toResult (Proxy :: Proxy b))
--     finalCols (a :*: b) = finalCols a ++ finalCols b
