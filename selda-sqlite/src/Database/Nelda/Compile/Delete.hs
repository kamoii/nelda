{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.Compile.Delete where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.Nelda.Compile.Quoting (quoteTableName)
import Database.Nelda.Compile.SQL (ppCol, runPP)
import Database.Nelda.Compile.TableFields (ToQueryFields, toQueryRow)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Types (Nullability(NonNull), Exp, Param, paramToSqlParam)
import Database.Nelda.Schema (Table (..), TableName)
import Database.Nelda.SqlType (SqlParam)
import Database.Nelda.Types (Sql (Sql))
import JRec

-- | Compile a @DELETE FROM@ query.
--
-- TODO: toTup は何をしている?必要なのか？
compileDelete ::
    (lts ~ ToQueryFields cols) =>
    Table name cols ->
    (Row s 'NonNull (Rec lts) -> Col s 'NonNull Bool) ->
    (Sql, [SqlParam])
compileDelete tbl@Table{tabName} check =
    (Sql statement, map paramToSqlParam params)
  where
    (statement, params) = unsafeCompileDelete tabName predicate
    One predicate = check $ toQueryRow tbl

-- -- | Compile a @DELETE@ statement.
unsafeCompileDelete :: TableName -> Exp Bool -> (Text, [Param])
unsafeCompileDelete name predicate = runPP ppDelete
  where
    ppDelete = do
        c' <- ppCol predicate
        pure $ Text.unwords ["DELETE FROM", quoteTableName name, "WHERE", c']
