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
import Data.Tup
import Data.Typeable (Typeable)
import Data.Data (Proxy(Proxy))
import Control.Monad (liftM2)

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

instance (SqlType a, Result b) => Result (Col s a :*: b) where
    type Res (Col s a :*: b) = a :*: Res b
    toResult _ = liftM2 (:*:) (fromSqlValue <$> next) (toResult (Proxy :: Proxy b))
    finalCols (a :*: b) = finalCols a ++ finalCols b

instance (ResultRow a, Result b) => Result (Row s a :*: b) where
    type Res (Row s a :*: b) = a :*: Res b
    toResult _ = liftM2 (:*:) nextResult (toResult (Proxy :: Proxy b))
    finalCols (a :*: b) = finalCols a ++ finalCols b

instance SqlType a => Result (Col s a) where
    type Res (Col s a) = a
    toResult _ = fromSqlValue <$> next
    finalCols (One c) = [Some c]

instance ResultRow a => Result (Row s a) where
    type Res (Row s a) = a
    toResult _ = nextResult
    finalCols (Many cs) = [Some c | Untyped c <- cs]
