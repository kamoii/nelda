{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.Query.Result where

import Data.Data (Proxy (Proxy))
import Database.Nelda.Query.ResultReader
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row (Many))
import Database.Nelda.SQL.Types (SomeCol (Some), UntypedCol (Untyped))
import Database.Nelda.SqlRowConversion (FromSqlRow, fromSqlValues')
import Database.Nelda.SqlType
import Database.Nelda.SqlTypeConversion (FromSqlType, fromSqlValue')

-- Query の結果を取り出すための type class
-- Col s a は SqlType を使って, Row s a は SqlRow 型クラスを使う

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Result a r | a -> r where
    -- | Converts the given list of @SqlValue@s into an tuple of well-typed
    --   results.
    --   See 'querySQLite' for example usage.
    toResult :: Proxy a -> ResultReader r

    -- | Produce a list of all columns present in the result.
    finalCols :: a -> [SomeCol]

buildResult :: Result a r => Proxy a -> [SqlValue] -> r
buildResult p = runResultReader (toResult p)
-- * Col/Row instance

-- Col s a の時であれば SqlType a 制約があれば十分だったけど nullability が定まらまない状況だと
-- Result (Col s n1 a) の制約も付けるよう要求されるようになった。
-- SqlType a である限り Nullability に関係せず Result (Col s a) を満すこと以下の定義でいいたいのだが,
-- GHC が理解してくれない...
--
instance FromSqlType n a r => Result (Col s n a) r where
    toResult _ = fromSqlValue' @n @a <$> next
    finalCols (One c) = [Some c]

instance FromSqlRow n a r => Result (Row s n a) r where
    toResult _ = fromSqlValues' @n @a
    finalCols (Many cs) = [Some c | Untyped c <- cs]

-- * Tuple instances

{-
TODO: formater と同じように

def tuple_instance(n)
  ix = (0...n)
  vars = ix.map {|i| "c#{i}" }
  ress = ix.map {|i| "r#{i}" }
  constraint = ix.map {|i| "Result c#{i} r#{i}"}.join(',')
  tuples_sec = "(#{vars.map{''}.join(',')})"
  to_tups = vars.map {|v| "_toTup @#{v}"}.join(' <*> ')
  from_tups = vars.map {|v| "_fromTup #{v}"}.join(' *> ')
  <<EOS
instance (#{constraint}, r ~ (#{ress.join(',')})) => Result (#{vars.join(',')}) r where
    toResult _ = #{tuples_sec} <$> #{vars.map{|v| "toResult (Proxy :: Proxy #{v})"}.join(' <*> ')}
    finalCols (#{vars.join(',')}) = #{vars.map{|v| "finalCols #{v}"}.join(' <> ')}
EOS
end

2.upto(8) { |n| puts tuple_instance(n) }
-}
instance (Result c0 r0, Result c1 r1, r ~ (r0, r1)) => Result (c0, c1) r where
    toResult _ = (,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1)
    finalCols (c0, c1) = finalCols c0 <> finalCols c1
instance (Result c0 r0, Result c1 r1, Result c2 r2, r ~ (r0, r1, r2)) => Result (c0, c1, c2) r where
    toResult _ = (,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2)
    finalCols (c0, c1, c2) = finalCols c0 <> finalCols c1 <> finalCols c2
instance (Result c0 r0, Result c1 r1, Result c2 r2, Result c3 r3, r ~ (r0, r1, r2, r3)) => Result (c0, c1, c2, c3) r where
    toResult _ = (,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3)
    finalCols (c0, c1, c2, c3) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3
instance (Result c0 r0, Result c1 r1, Result c2 r2, Result c3 r3, Result c4 r4, r ~ (r0, r1, r2, r3, r4)) => Result (c0, c1, c2, c3, c4) r where
    toResult _ = (,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4)
    finalCols (c0, c1, c2, c3, c4) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4
instance (Result c0 r0, Result c1 r1, Result c2 r2, Result c3 r3, Result c4 r4, Result c5 r5, r ~ (r0, r1, r2, r3, r4, r5)) => Result (c0, c1, c2, c3, c4, c5) r where
    toResult _ = (,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5)
    finalCols (c0, c1, c2, c3, c4, c5) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5
instance (Result c0 r0, Result c1 r1, Result c2 r2, Result c3 r3, Result c4 r4, Result c5 r5, Result c6 r6, r ~ (r0, r1, r2, r3, r4, r5, r6)) => Result (c0, c1, c2, c3, c4, c5, c6) r where
    toResult _ = (,,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5) <*> toResult (Proxy :: Proxy c6)
    finalCols (c0, c1, c2, c3, c4, c5, c6) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5 <> finalCols c6
instance (Result c0 r0, Result c1 r1, Result c2 r2, Result c3 r3, Result c4 r4, Result c5 r5, Result c6 r6, Result c7 r7, r ~ (r0, r1, r2, r3, r4, r5, r6, r7)) => Result (c0, c1, c2, c3, c4, c5, c6, c7) r where
    toResult _ = (,,,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5) <*> toResult (Proxy :: Proxy c6) <*> toResult (Proxy :: Proxy c7)
    finalCols (c0, c1, c2, c3, c4, c5, c6, c7) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5 <> finalCols c6 <> finalCols c7

-- instance (SqlType a, Result b) => Result (Col s a :*: b) where
--     type Res (Col s a :*: b) = a :*: Res b
--     toResult _ = liftM2 (:*:) (fromSqlValue <$> next) (toResult (Proxy :: Proxy b))
--     finalCols (a :*: b) = finalCols a ++ finalCols b

-- instance (SqlRow a, Result b) => Result (Row s a :*: b) where
--     type Res (Row s a :*: b) = a :*: Res b
--     toResult _ = liftM2 (:*:) nextResult (toResult (Proxy :: Proxy b))
--     finalCols (a :*: b) = finalCols a ++ finalCols b
