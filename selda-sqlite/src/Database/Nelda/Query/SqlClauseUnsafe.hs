{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Database.Nelda.Query.SqlClauseUnsafe where

import Data.Data (Proxy (Proxy))
import Data.String (IsString (fromString))
import Database.Nelda.Query.Monad (Query (..), addSource, rename, renameAll)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row (Many))
import Database.Nelda.SQL.Types (ColName, Exp (Col, Lit), QueryFragment (RawExp), SomeCol (Named), SqlSource (RawSql), UntypedCol (Untyped), hideRenaming, mkLit, sqlFrom)
import Database.Nelda.SqlRow (SqlRow, reflectRowGhost)
import Database.Nelda.SqlType (SqlType)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Database.Nelda.SQL.Nullability (Nullability(NonNull))

-- 結果の n や a は type inferece に任せるのは危険なため明示的に Proxy を渡す internface にする。
-- まあそれでも type inference に任せて単に Proxy を返すのは可能だが...

-- | Create a raw SQL query fragment from the given column.
inject :: Col s n a -> QueryFragment
inject (One x) = RawExp x

-- | Create a raw SQL query fragment from the given value.
injectLiteral :: SqlType a => a -> QueryFragment
injectLiteral = RawExp . Lit . mkLit

-- | Execute a raw SQL statement, returning a row consisting of columns by the
--   given names.
--   Will fail if the number of names given does not match up with
--   the type of the returned row.
--   Will generate invalid SQL if the given names don't match up with the
--   column names in the given query.
--
-- TODO: 結果Row の nullability を NonNull に固定している。Nullable 欲しい場合ってある？
rawQuery ::
    forall row s _rec.
    SqlRow row _rec =>
    Proxy row ->
    QueryFragment ->
    Query s (Row s 'NonNull row)
rawQuery _ q = Query $ do
    rns <- renameAll [Untyped (Col name) | name <- names]
    addSource $ sqlFrom rns (RawSql q)
    return (Many (map hideRenaming rns))
  where
    names = reflectRowGhost colName (Proxy @row)
    colName :: forall l _n _t. KnownSymbol l => Proxy '(l, _n, _t) -> ColName
    colName _ = fromString $ symbolVal (Proxy @l)

-- | As 'rawQuery', but returns only a single column. Same warnings still apply.
--
-- e.g. Unsafe.rawQuery1
--          (Proxy :: Proxy '("foo", 'NonNull, Text))
--          "SELECT name as foo FROM people"
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
