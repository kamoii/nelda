{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Database.Nelda.Query.Result where

import Control.Monad.State.Strict (MonadState (get))
import Data.Data (Proxy (Proxy))
import qualified Database.Nelda.Backend.Types as BE
import Database.Nelda.Query.ResultReader
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row (Many))
import Database.Nelda.SQL.Types (SomeCol (Some), UntypedCol (Untyped))
import Database.Nelda.SqlType
import Database.Nelda.SQL.Nullability

-- Query の結果を取り出すための type class
-- Col s a は SqlType を使って, Row s a は SqlRow 型クラスを使う

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Result r where
    type Res r :: *

    -- | Converts the given list of @SqlValue@s into an tuple of well-typed
    --   results.
    --   See 'querySQLite' for example usage.
    toResult :: Proxy r -> ResultReader (Res r)

    -- | Produce a list of all columns present in the result.
    finalCols :: r -> [SomeCol]

buildResult :: Result r => Proxy r -> [SqlValue] -> Res r
buildResult p = runResultReader (toResult p)

-- * Col/Row instance

-- Col s a の時であれば SqlType a 制約があれば十分だったけど nullability が定まらまない状況だと
-- Result (Col s n1 a) の制約も付けるよう要求されるようになった。
-- SqlType a である限り Nullability に関係せず Result (Col s a) を満すこと以下の定義でいいたいのだが,
-- GHC が理解してくれない...
--
instance SqlType a => Result (Col s 'NonNull a) where
    type Res (Col s 'NonNull a) = a
    toResult _ = fromSqlValue <$> next
    finalCols (One c) = [Some c]

instance SqlType a => Result (Col s 'Nullable a) where
    type Res (Col s 'Nullable a) = Maybe a
    toResult _ = fromSqlValue' <$> next
    finalCols (One c) = [Some c]

instance SqlRow a => Result (Row s 'NonNull a) where
    type Res (Row s 'NonNull a) = a
    toResult _ = nextResult
    finalCols (Many cs) = [Some c | Untyped c <- cs]

instance SqlRow a => Result (Row s 'Nullable a) where
    type Res (Row s 'Nullable a) = Maybe a

    -- Read ahead for SqlRow's length.
    -- If all value is NULL then return Nothing. Otherwize try to read a
    -- There is a pathologicay case where a's field are nullable.
    toResult _ = do
        xs <- ResultReader get
        if all BE.isSqlValueNull (take (nestedCols (Proxy :: Proxy a)) xs)
            then pure Nothing
            else Just <$> nextResult
    finalCols (Many cs) = [Some c | Untyped c <- cs]

-- * Tuple instances

{-
TODO: formater と同じように

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
instance (Result c0, Result c1) => Result (c0, c1) where
    type Res (c0, c1) = (Res c0, Res c1)
    toResult _ = (,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1)
    finalCols (c0, c1) = finalCols c0 <> finalCols c1
instance (Result c0, Result c1, Result c2) => Result (c0, c1, c2) where
    type Res (c0, c1, c2) = (Res c0, Res c1, Res c2)
    toResult _ = (,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2)
    finalCols (c0, c1, c2) = finalCols c0 <> finalCols c1 <> finalCols c2
instance (Result c0, Result c1, Result c2, Result c3) => Result (c0, c1, c2, c3) where
    type Res (c0, c1, c2, c3) = (Res c0, Res c1, Res c2, Res c3)
    toResult _ = (,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3)
    finalCols (c0, c1, c2, c3) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3
instance (Result c0, Result c1, Result c2, Result c3, Result c4) => Result (c0, c1, c2, c3, c4) where
    type Res (c0, c1, c2, c3, c4) = (Res c0, Res c1, Res c2, Res c3, Res c4)
    toResult _ = (,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4)
    finalCols (c0, c1, c2, c3, c4) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4
instance (Result c0, Result c1, Result c2, Result c3, Result c4, Result c5) => Result (c0, c1, c2, c3, c4, c5) where
    type Res (c0, c1, c2, c3, c4, c5) = (Res c0, Res c1, Res c2, Res c3, Res c4, Res c5)
    toResult _ = (,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5)
    finalCols (c0, c1, c2, c3, c4, c5) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5
instance (Result c0, Result c1, Result c2, Result c3, Result c4, Result c5, Result c6) => Result (c0, c1, c2, c3, c4, c5, c6) where
    type Res (c0, c1, c2, c3, c4, c5, c6) = (Res c0, Res c1, Res c2, Res c3, Res c4, Res c5, Res c6)
    toResult _ = (,,,,,,) <$> toResult (Proxy :: Proxy c0) <*> toResult (Proxy :: Proxy c1) <*> toResult (Proxy :: Proxy c2) <*> toResult (Proxy :: Proxy c3) <*> toResult (Proxy :: Proxy c4) <*> toResult (Proxy :: Proxy c5) <*> toResult (Proxy :: Proxy c6)
    finalCols (c0, c1, c2, c3, c4, c5, c6) = finalCols c0 <> finalCols c1 <> finalCols c2 <> finalCols c3 <> finalCols c4 <> finalCols c5 <> finalCols c6
instance (Result c0, Result c1, Result c2, Result c3, Result c4, Result c5, Result c6, Result c7) => Result (c0, c1, c2, c3, c4, c5, c6, c7) where
    type Res (c0, c1, c2, c3, c4, c5, c6, c7) = (Res c0, Res c1, Res c2, Res c3, Res c4, Res c5, Res c6, Res c7)
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
