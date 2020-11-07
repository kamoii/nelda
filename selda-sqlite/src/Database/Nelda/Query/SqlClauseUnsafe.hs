{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Query.SqlClauseUnsafe where

import Data.Data (Proxy (Proxy))
import Database.Nelda.Query.Monad (Query (..), addSource, rename, sources)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Types (ColName (ColName), Exp (Col), QueryFragment, SomeCol (Named), SqlSource (RawSql), UntypedCol (Untyped), sqlFrom)
import Database.Nelda.SqlRow (SqlRow)
import Database.Nelda.SqlType (SqlType)
import GHC.TypeLits (symbolVal, KnownSymbol)
import Data.String (IsString(fromString))

-- 結果の n や a は type inferece に任せるのは危険なため明示的に Proxy を渡す internface にする。
-- まあそれでも type inference に任せて単に Proxy を返すのは可能だが...

-- | Execute a raw SQL statement, returning a row consisting of columns by the
--   given names.
--   Will fail if the number of names given does not match up with
--   the type of the returned row.
--   Will generate invalid SQL if the given names don't match up with the
--   column names in the given query.
-- rawQuery ::
--     forall row n s _rec.
--     SqlRow row _rec =>
--     Proxy row ->
--     QueryFragment ->
--     Query s (Row s n row)
-- rawQuery _ q = Query $ do
--     rns <- renameAll [Untyped (Col name) | name <- names]
--     addSource $ sqlFrom rns (RawSql q)
--     return (Many (map hideRenaming rns))

-- | As 'rawQuery', but returns only a single column. Same warnings still apply.
--
-- e.g. Unsafe.rawQuery1 (Proxy @'("foo", 'NonNull, Text)) "SELECT name as foo FROM people"
rawQuery1 ::
    forall a n s l.
    (SqlType a, KnownSymbol l) =>
    Proxy '(l, n, a) ->
    QueryFragment ->
    Query s (Col s n a)
rawQuery1 _ q = Query $ do
    let colname = fromString $ symbolVal (Proxy @l)
    name' <- head <$> rename (Untyped (Col colname))
    addSource $ sqlFrom [name'] (RawSql q)
    case name' of
        Named n _ -> return (One (Col n))
        _ -> error "BUG: renaming did not rename"
