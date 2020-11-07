{-# LANGUAGE RankNTypes #-}

module Database.Nelda.Query.SqlClauseUnsafe where

import Data.Data (Proxy (Proxy))
import Database.Nelda.Query.Monad (Query (..), addSource, rename, sources)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Types (ColName (ColName), Exp (Col), QueryFragment, SomeCol (Named), SqlSource (RawSql), UntypedCol (Untyped), sqlFrom)
import Database.Nelda.SqlRow (SqlRow)
import Database.Nelda.SqlType (SqlType)

-- | Execute a raw SQL statement, returning a row consisting of columns by the
--   given names.
--   Will fail if the number of names given does not match up with
--   the type of the returned row.
--   Will generate invalid SQL if the given names don't match up with the
--   column names in the given query.
-- rawQuery :: forall row n s _r. SqlRow row _r => [ColName] -> QueryFragment -> Query s (Row s n row)
-- rawQuery names q
--     | length names /= nestedCols (Proxy :: Proxy a) = do
--         let err =
--                 concat
--                     [ "rawQuery: return type has "
--                     , show (nestedCols (Proxy :: Proxy a))
--                     , " columns, but only "
--                     , show (length names)
--                     , " names were given"
--                     ]
--         throw (UnsafeError err)
--     | otherwise = Query $ do
--         rns <- renameAll [Untyped (Col name) | name <- names]
--         addSource $ sqlFrom rns (RawSql q)
--         return (Many (map hideRenaming rns))

-- | As 'rawQuery', but returns only a single column. Same warnings still apply.
-- Its probably better to give explicit type by TypeApplication, like rawQuery @Int @'NoNull "...",
-- than to rely on type inference.
rawQuery1 :: forall a n s. SqlType a => ColName -> QueryFragment -> Query s (Col s n a)
rawQuery1 name q = Query $ do
    name' <- head <$> rename (Untyped (Col name))
    addSource $ sqlFrom [name'] (RawSql q)
    case name' of
        Named n _ -> return (One (Col n))
        _ -> error "BUG: renaming did not rename"
